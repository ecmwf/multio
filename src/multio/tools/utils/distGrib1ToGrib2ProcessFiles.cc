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

#include "eckit/message/Message.h"
#include "eckit/message/Reader.h"
#include "metkit/mars2grib/api/Mars2Grib.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/sink/DataSink.h"
#include "multio/tools/utils/CodesHandleToEckitMessage.h"
#include "multio/tools/utils/grib2MarsMisc.h"

namespace multio::distGrib1ToGrib2 {

namespace {

using grib2MarsMisc::ExtractedMsg;
using grib2MarsMisc::MessageDisposition;

std::string rankOutputPath(const std::string& outputPrefix, int rank) {
    return outputPrefix + ".rank" + std::to_string(rank) + ".grib2";
}

eckit::LocalConfiguration sinkConfigurationForRank(const eckit::LocalConfiguration& options, const std::string& outputPrefix,
                                                   int rank) {
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
        sinkConf.set("path", rankOutputPath(outputPrefix, rank));
    }

    return sinkConf;
}

std::unique_ptr<sink::DataSink> buildSink(const eckit::LocalConfiguration& options, const std::string& outputPrefix,
                                          int rank) {
    const auto sinkConf = sinkConfigurationForRank(options, outputPrefix, rank);
    config::MultioConfiguration multioConf(eckit::LocalConfiguration{}, config::LocalPeerTag::Client);
    config::ComponentConfiguration componentConf(sinkConf, multioConf);
    std::cerr << "rank " << rank << " building sink of type: " << sinkConf.getString("type") << std::endl;
    return sink::DataSinkFactory::instance().build(sinkConf.getString("type"), componentConf);
}

FileOutcome processOneFile(int rank, const std::string& file, const eckit::LocalConfiguration& options,
                           metkit::mars2grib::Mars2Grib& encoder, sink::DataSink& writer) {
    FileOutcome outcome;
    outcome.filename = file;
    outcome.nMessages = 0;
    outcome.nEncoded = 0;
    outcome.nFailEncode = 0;
    outcome.nCopied = 0;
    outcome.nFailArchive = 0;
    outcome.nSkipped = 0;
    outcome.nFailExtract = 0;

    try {

        using eckit::message::ValueRepresentation;
        eckit::message::Reader reader{file};
        eckit::message::Message msg;
        while ((msg = reader.next())) {
            ++outcome.nMessages;
            // std::cerr << "rank " << rank << ": Processing file: " << file << " nMessages: " << outcome.nMessages << std::endl;

            ExtractedMsg extracted = grib2MarsMisc::grib2MarsMisc(msg, options);


            switch (extracted.disposition) {
                case MessageDisposition::Encode: {
                    try {
                        auto encoded = encoder.encode(extracted.values, extracted.mars, extracted.misc);
                        writer.write(tools::utils::to_eckit_message(*encoded));
                        ++outcome.nEncoded;
                    }
                    catch (...) {
                        ++outcome.nFailEncode;
                    }
                    break;
                }
                case MessageDisposition::CopyGrib2Verbatim:
                case MessageDisposition::CopyExceptMatched:
                case MessageDisposition::CopyInvalidMessage:
                case MessageDisposition::CopyDiscipline192:
                case MessageDisposition::CopyTimespanNonPositive:
/*
                    try {
                        writer.write(msg);
                        ++outcome.nCopied;
                    }
                    catch (...) {
                        ++outcome.nFailArchive;
                    }
                    break;
*/
                case MessageDisposition::SkipExcluded:
                case MessageDisposition::SkipFilteredOut:
                case MessageDisposition::SkipInvalidMessage:
                case MessageDisposition::SkipDiscipline192:
                case MessageDisposition::SkipTimespanNonPositive:
                    ++outcome.nSkipped;
                    break;
                case MessageDisposition::FailToExtract:
                    ++outcome.nFailExtract;
                    break;
                case MessageDisposition::FailToEncode:
                    ++outcome.nFailEncode;
                    break;
                case MessageDisposition::FailToArchive:
                    ++outcome.nFailArchive;
                    break;
            }

        }

        //std::cerr << "rank " << rank << ": Processing file: " << file << " nMessages: " << outcome.nMessages << std::endl;

    }
    catch (...) {
        ++outcome.nFailExtract;
    }

    outcome.status = deriveFileStatus(outcome);
    return outcome;
}

}  // namespace

std::vector<FileOutcome> processLocalFiles(const std::vector<std::string>& files, const eckit::LocalConfiguration& options,
                                           const std::string& outputPrefix, int rank) {
    std::vector<FileOutcome> outcomes;
    outcomes.reserve(files.size());

    auto writer = buildSink(options, outputPrefix, rank);

    metkit::mars2grib::Mars2Grib encoder{};
    for (const auto& file : files) {
        FileOutcome outcome = processOneFile(rank, file, options, encoder, *writer);
        writer->flush();
        std::cerr << formatRankProgressLine(outcome, rank) << '\n';
        outcomes.push_back(std::move(outcome));
    }

    writer->flush();


    return outcomes;
}

}  // namespace multio::distGrib1ToGrib2
