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

#include <cstdint>
#include <string>
#include <vector>

namespace multio::distGrib1ToGrib2 {

enum class FileStatus : std::uint8_t {
    Success = 0,
    FailedExtract = 1,
    FailedEncode = 2,
    FailedArchive = 3,
    FailedMixed = 4,
    Partial = 5,
    Unknown = 255
};

struct FileOutcome {
    std::string filename;
    FileStatus status = FileStatus::Unknown;
    std::size_t nMessages = 0;
    std::size_t nEncoded = 0;
    std::size_t nCopied = 0;
    std::size_t nSkipped = 0;
    std::size_t nFailExtract = 0;
    std::size_t nFailEncode = 0;
    std::size_t nFailArchive = 0;
};

const char* toString(FileStatus status);
FileStatus deriveFileStatus(const FileOutcome& o);
std::string formatOutcomeLine(const FileOutcome& o);
std::string formatRankProgressLine(const FileOutcome& o, int rank);
std::string serializeOutcomesLog(const std::vector<FileOutcome>& outcomes);
void writeGlobalOutcomeLog(const std::string& payload, const std::string& outputFile);

}  // namespace multio::distGrib1ToGrib2
