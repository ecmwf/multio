/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/io/FileHandle.h"
#include "eckit/message/Message.h"

#include "multio/tools/utils/grib2MarsMisc.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::sink {
class DataSink;
}

namespace multio::grib1ToGrib2 {

enum class ScalarDebugBucket : std::size_t
{
    Converted = 0,
    ConvertedAndArchived,
    CopiedGrib2Verbatim,
    CopiedBecauseExceptMatched,
    CopiedInvalidInputMessage,
    CopiedDiscipline192,
    CopiedNonPositiveTimespan,
    SkippedExcluded,
    SkippedFilteredOut,
    SkippedInvalidInputMessage,
    SkippedDiscipline192,
    SkippedNonPositiveTimespan,
    FailedReadHandleNotMemory,
    FailedMessageClassification,
    FailedExceptMatchedGrib1,
    FailedMapGrib1ToGrib2,
    FailedEmptyValues,
    FailedOptionOverrides,
    FailedMappings,
    FailedMarsDefaults,
    FailedMarsValidation,
    FailedMiscDefaults,
    FailedMiscValidation,
    FailedSpectralComplexOverflowProtection,
    FailedFileRead,
    FailedEncode,
    FailedArchive,
    FailedUnknownException,
};

const char* toString(ScalarDebugBucket bucket);
ScalarDebugBucket bucketForOutcome(grib2MarsMisc::ExtractionOutcomeCode code);

class ScalarDebugOutputs {
public:
    explicit ScalarDebugOutputs(std::string prefix);
    void writeInputMessage(ScalarDebugBucket bucket, const eckit::message::Message& msg);
    void writeArchiveFailureEncoded(const metkit::codes::CodesHandle& handle);
    bool enabled() const;
    std::size_t bucketCount(ScalarDebugBucket bucket) const;
    std::string summary() const;

private:
    eckit::FileHandle& inputHandle(ScalarDebugBucket bucket);
    eckit::FileHandle& archiveFailureEncodedHandle();

    std::string prefix_;
    std::unordered_map<std::size_t, std::unique_ptr<eckit::FileHandle>> inputHandles_;
    std::unique_ptr<eckit::FileHandle> archiveFailureEncodedHandle_;
    std::unordered_map<std::size_t, std::size_t> bucketCounts_;
};

std::unique_ptr<sink::DataSink> buildArchiveProbeSink(const eckit::LocalConfiguration& sinkConf);

}  // namespace multio::grib1ToGrib2
