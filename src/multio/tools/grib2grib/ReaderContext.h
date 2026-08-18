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

enum class WorkUnitReaderMode : std::uint8_t
{
    EccodesStream = 0,
    CandidateBoundary,
};

struct ReaderContext {
    WorkUnitReaderMode mode = WorkUnitReaderMode::EccodesStream;
};

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
