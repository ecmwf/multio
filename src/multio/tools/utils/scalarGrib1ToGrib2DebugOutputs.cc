/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/scalarGrib1ToGrib2DebugOutputs.h"

#include <sstream>
#include <stdexcept>
#include <utility>

#include "eckit/filesystem/PathName.h"
#include "eckit/io/Buffer.h"
#include "metkit/codes/api/CodesAPI.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/sink/DataSink.h"

namespace multio::grib1ToGrib2 {

namespace {

using grib2MarsMisc::ExtractionOutcomeCode;

std::string inputBucketPath(const std::string& prefix, ScalarDebugBucket bucket) {
    return prefix + "." + toString(bucket) + ".input.grib";
}

std::string archiveFailureEncodedPath(const std::string& prefix) {
    return prefix + ".FailedArchive.encoded.grib2";
}

}  // namespace

const char* toString(ScalarDebugBucket bucket) {
    switch (bucket) {
        case ScalarDebugBucket::Converted:
            return "Converted";
        case ScalarDebugBucket::ConvertedAndArchived:
            return "ConvertedAndArchived";
        case ScalarDebugBucket::CopiedGrib2Verbatim:
            return "CopiedGrib2Verbatim";
        case ScalarDebugBucket::CopiedBecauseExceptMatched:
            return "CopiedBecauseExceptMatched";
        case ScalarDebugBucket::CopiedInvalidInputMessage:
            return "CopiedInvalidInputMessage";
        case ScalarDebugBucket::CopiedDiscipline192:
            return "CopiedDiscipline192";
        case ScalarDebugBucket::CopiedNonPositiveTimespan:
            return "CopiedNonPositiveTimespan";
        case ScalarDebugBucket::SkippedExcluded:
            return "SkippedExcluded";
        case ScalarDebugBucket::SkippedFilteredOut:
            return "SkippedFilteredOut";
        case ScalarDebugBucket::SkippedInvalidInputMessage:
            return "SkippedInvalidInputMessage";
        case ScalarDebugBucket::SkippedDiscipline192:
            return "SkippedDiscipline192";
        case ScalarDebugBucket::SkippedNonPositiveTimespan:
            return "SkippedNonPositiveTimespan";
        case ScalarDebugBucket::FailedReadHandleNotMemory:
            return "FailedReadHandleNotMemory";
        case ScalarDebugBucket::FailedMessageClassification:
            return "FailedMessageClassification";
        case ScalarDebugBucket::FailedExceptMatchedGrib1:
            return "FailedExceptMatchedGrib1";
        case ScalarDebugBucket::FailedMapGrib1ToGrib2:
            return "FailedMapGrib1ToGrib2";
        case ScalarDebugBucket::FailedEmptyValues:
            return "FailedEmptyValues";
        case ScalarDebugBucket::FailedOptionOverrides:
            return "FailedOptionOverrides";
        case ScalarDebugBucket::FailedMappings:
            return "FailedMappings";
        case ScalarDebugBucket::FailedMarsDefaults:
            return "FailedMarsDefaults";
        case ScalarDebugBucket::FailedMarsValidation:
            return "FailedMarsValidation";
        case ScalarDebugBucket::FailedMiscDefaults:
            return "FailedMiscDefaults";
        case ScalarDebugBucket::FailedMiscValidation:
            return "FailedMiscValidation";
        case ScalarDebugBucket::FailedSpectralComplexOverflowProtection:
            return "FailedSpectralComplexOverflowProtection";
        case ScalarDebugBucket::FailedFileRead:
            return "FailedFileRead";
        case ScalarDebugBucket::FailedEncode:
            return "FailedEncode";
        case ScalarDebugBucket::FailedArchive:
            return "FailedArchive";
        case ScalarDebugBucket::FailedUnknownException:
            return "FailedUnknownException";
    }
    return "FailedUnknownException";
}

ScalarDebugBucket bucketForOutcome(ExtractionOutcomeCode code) {
    switch (code) {
        case ExtractionOutcomeCode::ProcessedAndArchived:
            return ScalarDebugBucket::ConvertedAndArchived;
        case ExtractionOutcomeCode::CopyRequiredGrib2Verbatim:
            return ScalarDebugBucket::CopiedGrib2Verbatim;
        case ExtractionOutcomeCode::CopyRequiredExceptMatched:
            return ScalarDebugBucket::CopiedBecauseExceptMatched;
        case ExtractionOutcomeCode::CopyRequiredInvalidMessage:
            return ScalarDebugBucket::CopiedInvalidInputMessage;
        case ExtractionOutcomeCode::CopyRequiredDiscipline192:
            return ScalarDebugBucket::CopiedDiscipline192;
        case ExtractionOutcomeCode::CopyRequiredTimespanNonPositive:
            return ScalarDebugBucket::CopiedNonPositiveTimespan;
        case ExtractionOutcomeCode::SkipRequiredExcluded:
            return ScalarDebugBucket::SkippedExcluded;
        case ExtractionOutcomeCode::SkipRequiredFilteredOut:
            return ScalarDebugBucket::SkippedFilteredOut;
        case ExtractionOutcomeCode::SkipRequiredInvalidMessage:
            return ScalarDebugBucket::SkippedInvalidInputMessage;
        case ExtractionOutcomeCode::SkipRequiredDiscipline192:
            return ScalarDebugBucket::SkippedDiscipline192;
        case ExtractionOutcomeCode::SkipRequiredTimespanNonPositive:
            return ScalarDebugBucket::SkippedNonPositiveTimespan;
        case ExtractionOutcomeCode::ExtractFailedReadHandleNotMemory:
            return ScalarDebugBucket::FailedReadHandleNotMemory;
        case ExtractionOutcomeCode::ExtractFailedMessageClassification:
            return ScalarDebugBucket::FailedMessageClassification;
        case ExtractionOutcomeCode::ExtractFailedExceptMatchedGrib1:
            return ScalarDebugBucket::FailedExceptMatchedGrib1;
        case ExtractionOutcomeCode::ExtractFailedMapGrib1ToGrib2:
            return ScalarDebugBucket::FailedMapGrib1ToGrib2;
        case ExtractionOutcomeCode::ExtractFailedEmptyValues:
            return ScalarDebugBucket::FailedEmptyValues;
        case ExtractionOutcomeCode::ExtractFailedOptionOverrides:
            return ScalarDebugBucket::FailedOptionOverrides;
        case ExtractionOutcomeCode::ExtractFailedMappings:
            return ScalarDebugBucket::FailedMappings;
        case ExtractionOutcomeCode::ExtractFailedMarsDefaults:
            return ScalarDebugBucket::FailedMarsDefaults;
        case ExtractionOutcomeCode::ExtractFailedMarsValidation:
            return ScalarDebugBucket::FailedMarsValidation;
        case ExtractionOutcomeCode::ExtractFailedMiscDefaults:
            return ScalarDebugBucket::FailedMiscDefaults;
        case ExtractionOutcomeCode::ExtractFailedMiscValidation:
            return ScalarDebugBucket::FailedMiscValidation;
        case ExtractionOutcomeCode::ExtractFailedSpectralComplexOverflowProtection:
            return ScalarDebugBucket::FailedSpectralComplexOverflowProtection;
        case ExtractionOutcomeCode::ExtractFailedFileRead:
            return ScalarDebugBucket::FailedFileRead;
        case ExtractionOutcomeCode::EncodeFailedMars2Grib:
            return ScalarDebugBucket::FailedEncode;
        case ExtractionOutcomeCode::ArchiveFailedSinkWrite:
            return ScalarDebugBucket::FailedArchive;
        case ExtractionOutcomeCode::ExtractFailedUnknownException:
        case ExtractionOutcomeCode::ReadyToEncode:
            return ScalarDebugBucket::FailedUnknownException;
    }
    return ScalarDebugBucket::FailedUnknownException;
}

namespace {

void cleanupExistingDebugOutputs(const std::string& prefix) {
    if (prefix.empty()) {
        return;
    }

    for (std::size_t i = 0; i <= static_cast<std::size_t>(ScalarDebugBucket::FailedUnknownException); ++i) {
        eckit::PathName path{inputBucketPath(prefix, static_cast<ScalarDebugBucket>(i))};
        if (path.exists()) {
            path.unlink();
        }
    }

    eckit::PathName encodedPath{archiveFailureEncodedPath(prefix)};
    if (encodedPath.exists()) {
        encodedPath.unlink();
    }
}

}  // namespace

ScalarDebugOutputs::ScalarDebugOutputs(std::string prefix) : prefix_(std::move(prefix)) {
    cleanupExistingDebugOutputs(prefix_);
}

void ScalarDebugOutputs::writeInputMessage(ScalarDebugBucket bucket, const eckit::message::Message& msg) {
    if (!enabled()) {
        return;
    }
    msg.write(inputHandle(bucket));
    ++bucketCounts_[static_cast<std::size_t>(bucket)];
}

void ScalarDebugOutputs::writeArchiveFailureEncoded(const metkit::codes::CodesHandle& handle) {
    if (!enabled()) {
        return;
    }
    eckit::Buffer buf{handle.messageSize()};
    handle.copyInto(reinterpret_cast<uint8_t*>(buf.data()), buf.size());
    archiveFailureEncodedHandle().write(buf.data(), buf.size());
}

bool ScalarDebugOutputs::enabled() const {
    return !prefix_.empty();
}

std::size_t ScalarDebugOutputs::bucketCount(ScalarDebugBucket bucket) const {
    if (const auto it = bucketCounts_.find(static_cast<std::size_t>(bucket)); it != bucketCounts_.end()) {
        return it->second;
    }
    return 0;
}

std::string ScalarDebugOutputs::summary() const {
    std::ostringstream out;
    bool first = true;
    for (std::size_t i = 0; i <= static_cast<std::size_t>(ScalarDebugBucket::FailedUnknownException); ++i) {
        const auto count = bucketCount(static_cast<ScalarDebugBucket>(i));
        if (count == 0) {
            continue;
        }
        if (!first) {
            out << ' ';
        }
        first = false;
        out << toString(static_cast<ScalarDebugBucket>(i)) << '=' << count;
    }
    if (first) {
        out << "none";
    }
    return out.str();
}

eckit::FileHandle& ScalarDebugOutputs::inputHandle(ScalarDebugBucket bucket) {
    const std::size_t key = static_cast<std::size_t>(bucket);
    auto& entry = inputHandles_[key];
    if (!entry) {
        entry = std::make_unique<eckit::FileHandle>(inputBucketPath(prefix_, bucket), true);
        entry->openForWrite(0);
    }
    return *entry;
}

eckit::FileHandle& ScalarDebugOutputs::archiveFailureEncodedHandle() {
    if (!archiveFailureEncodedHandle_) {
        archiveFailureEncodedHandle_ = std::make_unique<eckit::FileHandle>(archiveFailureEncodedPath(prefix_), true);
        archiveFailureEncodedHandle_->openForWrite(0);
    }
    return *archiveFailureEncodedHandle_;
}

std::unique_ptr<sink::DataSink> buildArchiveProbeSink(const eckit::LocalConfiguration& sinkConf) {
    if (!sinkConf.has("type")) {
        throw std::runtime_error("sink configuration is missing required key: type");
    }
    config::MultioConfiguration multioConf(eckit::LocalConfiguration{}, config::LocalPeerTag::Client);
    config::ComponentConfiguration componentConf(sinkConf, multioConf);
    return sink::DataSinkFactory::instance().build(sinkConf.getString("type"), componentConf);
}

}  // namespace multio::grib1ToGrib2
