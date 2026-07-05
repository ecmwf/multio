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

#include <array>
#include <cstdint>
#include <string>
#include <vector>

#include "multio/tools/utils/grib2MarsMisc.h"

namespace multio::distGrib1ToGrib2 {

enum class FileStatus : std::uint8_t {
    Success = 0,
    Partial = 1,
    ExtractFail = 2,
    EncodeFail = 3,
    ArchiveFail = 4,
    Fail = 5,
    Unknown = 255
};

struct FileOutcome {
    std::string filename;
    std::size_t nMessages = 0;
    std::array<std::size_t, static_cast<std::size_t>(grib2MarsMisc::ExtractionOutcomeCode::ExtractFailedUnknownException)
                                 + 1>
        outcomeCounters{};
};

const char* toString(FileStatus status);
const char* toString(grib2MarsMisc::ExtractionOutcomeCode code);
FileStatus deriveFileStatus(const FileOutcome& o);
std::string formatOutcomeLine(const FileOutcome& o);
std::string formatRankProgressLine(const FileOutcome& o, int rank);
std::string timestampString();

}  // namespace multio::distGrib1ToGrib2
