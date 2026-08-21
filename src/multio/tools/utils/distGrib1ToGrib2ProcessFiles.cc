/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/distGrib1ToGrib2ProcessFiles.h"

#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "eckit/filesystem/PathName.h"
#include "eckit/message/Message.h"
#include "eckit/message/Reader.h"
#include "metkit/mars2grib/api/Mars2Grib.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/sink/DataSink.h"
#include "multio/tools/utils/distGrib1ToGrib2Logging.h"
#include "multio/tools/utils/distGrib1ToGrib2ProcessOneMessage.h"
#include "multio/tools/utils/grib2MarsMisc.h"

namespace multio::distGrib1ToGrib2 {

namespace {

std::string rankOutputPath(const std::string& outputDirectory, int rank) {
    return outputDirectory + "/output/rank" + std::to_string(rank) + ".grib2";
}

eckit::LocalConfiguration sinkConfigurationForRank(const eckit::LocalConfiguration& options,
                                                   const std::string& outputDirectory, int rank) {
    eckit::LocalConfiguration sinkConf;
    if (options.has("sink")) {
        sinkConf = options.getSubConfiguration("sink");
    }
    else {
        sinkConf.set("type", std::string{"file"});
    }

    if (!sinkConf.has("type")) {
        sinkConf.set("type", std::string{"file"});
    }
    if (sinkConf.getString("type") == "file" && !sinkConf.has("path")) {
        sinkConf.set("path", rankOutputPath(outputDirectory, rank));
    }

    return sinkConf;
}

std::unique_ptr<sink::DataSink> buildSink(const eckit::LocalConfiguration& options, const std::string& outputDirectory,
                                          int rank) {
    const auto sinkConf = sinkConfigurationForRank(options, outputDirectory, rank);
    config::MultioConfiguration multioConf(eckit::LocalConfiguration{}, config::LocalPeerTag::Client);
    config::ComponentConfiguration componentConf(sinkConf, multioConf);
    if (sinkConf.getString("type") == "file" && sinkConf.has("path")) {
        eckit::PathName{sinkConf.getString("path")}.dirName().mkdir();
    }
    std::cerr << timestampString() << "rank " << rank << " building sink of type: " << sinkConf.getString("type")
              << std::endl;
    return sink::DataSinkFactory::instance().build(sinkConf.getString("type"), componentConf);
}

FileOutcome processOneFile(int rank, const std::string& file, const grib2MarsMisc::Grib2MarsMiscOptions& options,
                           metkit::mars2grib::Mars2Grib& encoder, sink::DataSink& writer) {
    (void)rank;
    FileOutcome outcome;
    outcome.filename = file;

    try {
        eckit::message::Reader reader{file};
        eckit::message::Message msg;
        while ((msg = reader.next())) {
            processOneMessage(msg, options, encoder, writer, outcome);
        }
    }
    catch (const std::exception&) {
        ++outcome
              .outcomeCounters[static_cast<std::size_t>(grib2MarsMisc::ExtractionOutcomeCode::ExtractFailedFileRead)];
        std::cerr
            << timestampString()
            << "DISCLAIMER: This code is designed to classify errors. All errors are trapped and the code continues."
            << std::endl;
    }
    catch (...) {
        ++outcome
              .outcomeCounters[static_cast<std::size_t>(grib2MarsMisc::ExtractionOutcomeCode::ExtractFailedFileRead)];
        std::cerr
            << timestampString()
            << "DISCLAIMER: This code is designed to classify errors. All errors are trapped and the code continues."
            << std::endl;
    }

    return outcome;
}

}  // namespace

std::vector<FileOutcome> processLocalFiles(const std::vector<std::string>& files,
                                           const grib2MarsMisc::Grib2MarsMiscOptions& grib2MarsMiscOptions,
                                           const eckit::LocalConfiguration& rawOptions,
                                           const std::string& outputDirectory, int rank) {
    std::vector<FileOutcome> outcomes;
    outcomes.reserve(files.size());

    auto writer = buildSink(rawOptions, outputDirectory, rank);

    metkit::mars2grib::Mars2Grib encoder{};
    for (const auto& file : files) {
        FileOutcome outcome = processOneFile(rank, file, grib2MarsMiscOptions, encoder, *writer);
        writer->flush();
        std::cerr << timestampString() << formatRankProgressLine(outcome, rank) << '\n';
        outcomes.push_back(std::move(outcome));
    }

    writer->flush();

    return outcomes;
}

}  // namespace multio::distGrib1ToGrib2
