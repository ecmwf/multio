/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <exception>
#include <iostream>
#include <string>

#include <mpi.h>

#include "eckit/runtime/Main.h"

#include "multio/tools/utils/distGrib1ToGrib2LoadBalancer.h"
#include "multio/tools/utils/distGrib1ToGrib2Logging.h"
#include "multio/tools/utils/distGrib1ToGrib2Mpi.h"
#include "multio/tools/utils/distGrib1ToGrib2Options.h"
#include "multio/tools/utils/distGrib1ToGrib2OutcomesReport.h"
#include "multio/tools/utils/distGrib1ToGrib2ProcessFiles.h"
#include "multio/tools/utils/grib2MarsMisc.h"


int main(int argc, char** argv) {
    using namespace multio::distGrib1ToGrib2;

    eckit::Main::initialise( argc, argv, "MULTIO_HOME");

    MPI_Init(&argc, &argv);

    int rank = 0;
    int worldSize = 0;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &worldSize);


    try {
        if (argc != 4) {
            if (rank == 0) {
                std::cerr << "Usage:\n"
                          << "  mpirun -np <N> " << argv[0] << " <input_file.list> <output_prefix> <options.yaml>\n\n"
                          << "Outputs:\n"
                          << "  <output_prefix>_chunk_report.csv\n"
                          << "  <output_prefix>_GlobalOutcome.log\n"
                          << "  <output_prefix>.rank<R>.grib2\n";
            }
            MPI_Finalize();
            return 2;
        }

        const std::string inputList = argv[1];
        const std::string outputPrefix = argv[2];
        const std::string optionsYaml = argv[3];

        std::vector<std::string> localFiles;
        if (rank == 0) {
            auto files = loadFileListWithSizes(inputList);
            if (files.empty()) {
                throw std::runtime_error("input list contains no valid files");
            }

            auto result = makeBalancedChunks(std::move(files), static_cast<std::size_t>(worldSize));
            const std::string reportFile = outputPrefix + "_chunk_report.csv";
            writeChunkReport(result, reportFile);
            printSplitSummaryToStderr(result);
            std::cerr << timestampString() << "chunk report written to: " << reportFile << '\n';

            for (int dest = 1; dest < worldSize; ++dest) {
                sendFileListToRank(result.chunks[static_cast<std::size_t>(dest)], dest, MPI_COMM_WORLD);
            }
            localFiles = std::move(result.chunks[0]);
        }
        else {
            localFiles = recvFileListFromRank0(MPI_COMM_WORLD);
        }

        const eckit::LocalConfiguration options = loadAndBroadcastOptions(rank, optionsYaml, MPI_COMM_WORLD);
        const auto grib2MarsMiscOptions = multio::grib2MarsMisc::makeGrib2MarsMiscOptions(options);

        std::cerr << timestampString() << "rank " << rank << " received " << localFiles.size() << " files" << std::endl;
        const std::vector<FileOutcome> localOutcomes
            = processLocalFiles(localFiles, grib2MarsMiscOptions, options, outputPrefix, rank);

        std::cerr << timestampString() << "rank " << rank << " processed all the files" << std::endl;
        MPI_Barrier(MPI_COMM_WORLD);


        const std::string globalOutcomesPayload
            = gatherStringToRank0(serializeFileOutcomes(localOutcomes), rank, worldSize, MPI_COMM_WORLD);

        if (rank == 0) {
            const auto globalOutcomes = deserializeFileOutcomes(globalOutcomesPayload);
            const auto reportPaths = makeReportPaths(outputPrefix);
            writeOutcomeReports(globalOutcomes, reportPaths);
            std::cerr << timestampString() << "global outcome written to: " << reportPaths.perFileLog << '\n';
        }

        MPI_Finalize();
        return 0;
    }
    catch (const std::exception& e) {
        std::cerr << "ERROR on rank " << rank << ": " << e.what() << '\n';
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }
    catch (...) {
        std::cerr << "ERROR on rank " << rank << ": unknown exception\n";
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }
}
