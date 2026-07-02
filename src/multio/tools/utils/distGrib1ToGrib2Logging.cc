/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/distGrib1ToGrib2Logging.h"

#include <fstream>
#include <sstream>
#include <stdexcept>

namespace multio::distGrib1ToGrib2 {

namespace {

std::size_t successfulMessages(const FileOutcome& o) {
    return o.nEncoded + o.nCopied;
}

std::string quoteForLog(const std::string& s) {
    std::string out;
    out.reserve(s.size() + 2);
    out.push_back('"');
    for (char c : s) {
        if (c == '"') {
            out += "\\\"";
        }
        else {
            out.push_back(c);
        }
    }
    out.push_back('"');
    return out;
}

}  // namespace

const char* toString(FileStatus status) {
    switch (status) {
        case FileStatus::Success:
            return "Success";
        case FileStatus::FailedExtract:
            return "FailedExtract";
        case FileStatus::FailedEncode:
            return "FailedEncode";
        case FileStatus::FailedArchive:
            return "FailedArchive";
        case FileStatus::FailedMixed:
            return "FailedMixed";
        case FileStatus::Partial:
            return "Partial";
        case FileStatus::Unknown:
            return "Unknown";
    }
    return "Unknown";
}

FileStatus deriveFileStatus(const FileOutcome& o) {
    const auto nSuccess = successfulMessages(o);
    if (o.nFailExtract > 0 && nSuccess == 0 && o.nFailEncode == 0 && o.nFailArchive == 0) {
        return FileStatus::FailedExtract;
    }
    if (o.nFailEncode == 0 && o.nFailArchive == 0 && o.nFailExtract == 0) {
        return FileStatus::Success;
    }
    if (nSuccess > 0 || o.nSkipped > 0) {
        return FileStatus::Partial;
    }
    if (o.nFailEncode > 0 && o.nFailArchive > 0) {
        return FileStatus::FailedMixed;
    }
    if (o.nFailEncode > 0) {
        return FileStatus::FailedEncode;
    }
    if (o.nFailArchive > 0) {
        return FileStatus::FailedArchive;
    }
    return FileStatus::Unknown;
}

std::string formatOutcomeLine(const FileOutcome& o) {
    std::ostringstream out;
    out << '[' << toString(o.status) << "] " << quoteForLog(o.filename) << ", " << o.nMessages << ", " << o.nEncoded << ", "
        << o.nCopied << ", " << o.nSkipped << ", " << o.nFailExtract << ", " << o.nFailEncode << ", "
        << o.nFailArchive << '\n';
    return out.str();
}

std::string formatRankProgressLine(const FileOutcome& o, int rank) {
    std::ostringstream out;
    out << "rank " << rank << " processed " << o.filename << " status=" << toString(o.status) << " NMessages=" << o.nMessages
        << " NEncoded=" << o.nEncoded << " NCopied=" << o.nCopied << " NSkipped=" << o.nSkipped
        << " NFAIL_Extract=" << o.nFailExtract << " NFAIL_Encode=" << o.nFailEncode << " NFAIL_Archive=" << o.nFailArchive;
    return out.str();
}

std::string serializeOutcomesLog(const std::vector<FileOutcome>& outcomes) {
    std::ostringstream out;
    for (const auto& outcome : outcomes) {
        out << formatOutcomeLine(outcome);
    }
    return out.str();
}

void writeGlobalOutcomeLog(const std::string& payload, const std::string& outputFile) {
    std::ofstream out(outputFile);
    if (!out) {
        throw std::runtime_error("cannot open output file: " + outputFile);
    }

    out << "# [Status] fileName, NMessages, NEncoded, NCopied, NSkipped, NFAIL_Extract, NFAIL_Encode, NFAIL_Archive\n";
    out << payload;
}

}  // namespace multio::distGrib1ToGrib2
