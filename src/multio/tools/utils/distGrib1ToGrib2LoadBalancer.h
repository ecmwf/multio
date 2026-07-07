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

#include <string>
#include <utility>
#include <vector>

namespace multio::distGrib1ToGrib2 {

using FileWithSize = std::pair<std::string, long>;

struct SplitResult {
    std::vector<std::vector<std::string>> chunks;
    std::vector<long long> weights;
};

long fileSizeBytes(const std::string& path);
std::vector<FileWithSize> loadFileListWithSizes(const std::string& listFile);
SplitResult makeBalancedChunks(std::vector<FileWithSize> files, std::size_t nChunks);
void writeChunkReport(const SplitResult& result, const std::string& reportFile);
void printSplitSummaryToStderr(const SplitResult& result);

}  // namespace multio::distGrib1ToGrib2
