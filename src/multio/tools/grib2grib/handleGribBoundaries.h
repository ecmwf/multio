/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Helpers for scanning likely GRIB message boundaries before ecCodes validation.
///
/// These helpers implement a two-step boundary strategy:
/// - search for candidate `GRIB` starts only inside an owned start range
/// - validate the full candidate message against physical EOF and ecCodes

#pragma once

#include <cstdint>
#include <cstdio>
#include <optional>
#include <string>

namespace multio::distGrib1ToGrib2::grib2grib {

struct CandidateMessage {
    off_t offset = 0;
    std::uint64_t length = 0;
};

std::optional<CandidateMessage> searchCandidateMessage(std::FILE* file, const std::string& filename,
                                                       off_t searchOffset, off_t endOffset, off_t fileEndOffset);

}  // namespace multio::distGrib1ToGrib2::grib2grib
