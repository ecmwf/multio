/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/distGrib1ToGrib2ProcessOneMessage.h"

#include "eckit/message/Message.h"
#include "metkit/mars2grib/api/Mars2Grib.h"

#include "multio/sink/DataSink.h"
#include "multio/tools/utils/CodesHandleToEckitMessage.h"

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

}  // namespace

void processOneMessage(const eckit::message::Message& msg, const grib2MarsMisc::Grib2MarsMiscOptions& options,
                       metkit::mars2grib::Mars2Grib& encoder, sink::DataSink& writer, FileOutcome& outcome) {
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
        case MessageDisposition::ComplexExclusion:
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

}  // namespace multio::distGrib1ToGrib2