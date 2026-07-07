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
#include "multio/tools/utils/CodesHandleToEckitMessage.h"
#include "multio/tools/utils/distGrib1ToGrib2Logging.h"
#include "multio/tools/utils/grib2MarsMisc.h"

namespace multio::distGrib1ToGrib2 {

namespace {

using grib2MarsMisc::ExtractedMsg;
using grib2MarsMisc::ExtractionOutcome;
using grib2MarsMisc::ExtractionOutcomeCode;
using grib2MarsMisc::MessageDisposition;

constexpr std::size_t outcomeIndex(ExtractionOutcomeCode code) {
    return static_cast<std::size_t>(code);
}

void bumpOutcome(FileOutcome& outcome, ExtractionOutcomeCode code) {
    ++outcome.outcomeCounters[outcomeIndex(code)];
}

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
            ++outcome.nMessages;

            auto result = grib2MarsMisc::grib2MarsMisc(msg, options);
            ExtractedMsg& extracted = result.extractedMessage;
            const ExtractionOutcome& extractionOutcome = result.extractionOutcome;

            switch (extractionOutcome.disposition) {
                case MessageDisposition::Encode: {
                    auto encoded = decltype(encoder.encode(extracted.values, extracted.mars, extracted.misc)){};
                    try {
                        encoded = encoder.encode(extracted.values, extracted.mars, extracted.misc);
                    }
                    catch (const std::exception&) {
                        bumpOutcome(outcome, ExtractionOutcomeCode::EncodeFailedMars2Grib);
                        std::cerr << timestampString()
                                  << "DISCLAIMER: This code is designed to classify errors. All errors are trapped and "
                                     "the code continues."
                                  << std::endl;
                        break;
                    }
                    catch (...) {
                        bumpOutcome(outcome, ExtractionOutcomeCode::EncodeFailedMars2Grib);
                        std::cerr << timestampString()
                                  << "DISCLAIMER: This code is designed to classify errors. All errors are trapped and "
                                     "the code continues."
                                  << std::endl;
                        break;
                    }

                    try {
                        writer.write(tools::utils::to_eckit_message(*encoded));
                        bumpOutcome(outcome, ExtractionOutcomeCode::ProcessedAndArchived);
                    }
                    catch (const std::exception&) {
                        std::cerr << timestampString()
                                  << "DISCLAIMER: This code is designed to classify errors. All errors are trapped and "
                                     "the code continues."
                                  << std::endl;
                        bumpOutcome(outcome, ExtractionOutcomeCode::ArchiveFailedSinkWrite);
                    }
                    catch (...) {
                        std::cerr << timestampString()
                                  << "DISCLAIMER: This code is designed to classify errors. All errors are trapped and "
                                     "the code continues."
                                  << std::endl;
                        bumpOutcome(outcome, ExtractionOutcomeCode::ArchiveFailedSinkWrite);
                    }

                    break;
                }
                case MessageDisposition::CopyGrib2Verbatim:
                case MessageDisposition::CopyExceptMatched:
                case MessageDisposition::CopyInvalidMessage:
                case MessageDisposition::CopyDiscipline192:
                case MessageDisposition::CopyTimespanNonPositive:
                case MessageDisposition::SkipExcluded:
                case MessageDisposition::SkipFilteredOut:
                case MessageDisposition::SkipInvalidMessage:
                case MessageDisposition::SkipDiscipline192:
                case MessageDisposition::SkipTimespanNonPositive:
                    bumpOutcome(outcome, extractionOutcome.code);
                    break;
                case MessageDisposition::FailToExtract:
                    bumpOutcome(outcome, extractionOutcome.code);
                    break;
                case MessageDisposition::FailToEncode:
                    bumpOutcome(outcome, ExtractionOutcomeCode::EncodeFailedMars2Grib);
                    break;
                case MessageDisposition::FailToArchive:
                    bumpOutcome(outcome, ExtractionOutcomeCode::ArchiveFailedSinkWrite);
                    break;
            }
        }
    }
    catch (const std::exception&) {
        bumpOutcome(outcome, ExtractionOutcomeCode::ExtractFailedFileRead);
        std::cerr
            << timestampString()
            << "DISCLAIMER: This code is designed to classify errors. All errors are trapped and the code continues."
            << std::endl;
    }
    catch (...) {
        bumpOutcome(outcome, ExtractionOutcomeCode::ExtractFailedFileRead);
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
