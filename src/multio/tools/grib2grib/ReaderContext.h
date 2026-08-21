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
/// @brief Reader-mode configuration for `grib2grib` work-unit iteration.

#pragma once

#include <cstdint>
#include <string>

#include "eckit/exception/Exceptions.h"

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Runtime strategy used by `UnitOfWork` to locate owned GRIB messages.
///
/// `EccodesStream` preserves the historical probing behavior where ecCodes is
/// asked to scan the file stream directly from the coarse offset.
///
/// `CandidateBoundary` first scans for candidate `GRIB` starts inside the owned
/// start range and then validates complete messages against physical EOF before
/// decoding them from memory.
enum class WorkUnitReaderMode : std::uint8_t
{
    EccodesStream = 0,
    CandidateBoundary,
};

/// @brief Parsed reader-specific runtime configuration.
///
/// The `reader` section is intentionally small. Today it contains only the
/// reader mode. The structure still exists as a dedicated context so more
/// reader-local policies can be added later without changing `GlobalContext`.
struct ReaderContext {
    WorkUnitReaderMode mode = WorkUnitReaderMode::EccodesStream;
};

/// @brief Parse one textual reader-mode spelling used by YAML and CLI options.
/// @param mode String value such as `eccodes-stream` or `candidate-boundary`.
/// @return Parsed reader-mode enum.
/// @throw eckit::BadValue If the string does not map to a supported mode.
inline WorkUnitReaderMode parseWorkUnitReaderMode(const std::string& mode) {
    if (mode == "eccodes-stream") {
        return WorkUnitReaderMode::EccodesStream;
    }

    if (mode == "candidate-boundary") {
        return WorkUnitReaderMode::CandidateBoundary;
    }

    throw eckit::BadValue("Unsupported reader mode: " + mode, Here());
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
