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

#include "eckit/filesystem/PathName.h"
#include "eckit/mpi/Comm.h"
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

    eckit::Main::initialise(argc, argv, "MULTIO_HOME");

    // eckit::mpi lazily initialises MPI on first access and registers a finaliser at exit.
    eckit::mpi::Comm& comm = eckit::mpi::comm();

    const int rank = static_cast<int>(comm.rank());
    const int worldSize = static_cast<int>(comm.size());


    try {
        if (argc != 4) {
            if (rank == 0) {
                std::cerr << "Usage:\n"
                          << "  mpirun -np <N> " << argv[0]
                          << " <input_file.list> <output_directory> <options.yaml>\n\n"
                          << "Outputs:\n"
                          << "  <output_directory>/Summary.log\n"
                          << "  <output_directory>/Summary.json\n"
                          << "  <output_directory>/output/...\n"
                          << "  <output_directory>/logging/...\n";
            }
            return 2;
        }

        const std::string inputList = argv[1];
        const std::string outputDirectory = argv[2];
        const std::string optionsYaml = argv[3];
        const eckit::LocalConfiguration options = loadAndBroadcastOptions(optionsYaml, comm);
        const auto reportPaths = makeReportPaths(outputDirectory);

        std::vector<std::string> localFiles;
        if (rank == 0) {
            eckit::PathName{outputDirectory}.mkdir();
            eckit::PathName{outputDirectory + "/output"}.mkdir();
            eckit::PathName{reportPaths.loggingDirectory}.mkdir();

            auto files = loadFileListWithSizes(inputList);
            if (files.empty()) {
                throw std::runtime_error("input list contains no valid files");
            }

            auto result = makeBalancedChunks(std::move(files), static_cast<std::size_t>(worldSize));
            const std::string reportFile = reportPaths.loggingDirectory + "/chunk_report.csv";
            writeChunkReport(result, reportFile);
            printSplitSummaryToStderr(result);
            std::cerr << timestampString() << "chunk report written to: " << reportFile << '\n';

            for (int dest = 1; dest < worldSize; ++dest) {
                sendFileListToRank(result.chunks[static_cast<std::size_t>(dest)], dest, comm);
            }
            localFiles = std::move(result.chunks[0]);
        }
        else {
            localFiles = recvFileListFromRank0(comm);
        }

        const auto grib2MarsMiscOptions = multio::grib2MarsMisc::makeGrib2MarsMiscOptions(options);

        std::cerr << timestampString() << "rank " << rank << " received " << localFiles.size() << " files" << std::endl;
        const std::vector<FileOutcome> localOutcomes
            = processLocalFiles(localFiles, grib2MarsMiscOptions, options, outputDirectory, rank);

        std::cerr << timestampString() << "rank " << rank << " processed all the files" << std::endl;
        comm.barrier();


        const std::string globalOutcomesPayload
            = gatherStringToRank0(serializeFileOutcomes(localOutcomes), rank, worldSize, comm);

        if (rank == 0) {
            const auto globalOutcomes = deserializeFileOutcomes(globalOutcomesPayload);
            writeOutcomeReports(globalOutcomes, reportPaths);
            std::cerr << timestampString() << "summary written to: " << reportPaths.summaryLog << '\n';
            std::cerr << timestampString() << "json summary written to: " << reportPaths.summaryJson << '\n';
        }

        return 0;
    }
    catch (const std::exception& e) {
        std::cerr << "ERROR on rank " << rank << ": " << e.what() << '\n';
        comm.abort(1);
        return 1;
    }
    catch (...) {
        std::cerr << "ERROR on rank " << rank << ": unknown exception\n";
        comm.abort(1);
        return 1;
    }
}
