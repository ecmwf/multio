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
/// @brief Standalone `MarsBasedFilter` stage implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/stages/MarsBasedFilter.h"

namespace multio::distGrib1ToGrib2::grib2grib {

void validateMarsBasedFilterContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }
}

MarsBasedFilterContext parseMarsBasedFilterContext(const eckit::LocalConfiguration& config) {
    MarsBasedFilterContext parsed;

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    return parsed;
}

void freeMarsBasedFilterContext(MarsBasedFilterContext& context) noexcept {
    (void)context;
}

MarsBasedFilterCode runMarsBasedFilterStage(const eckit::LocalConfiguration& mars,
                                            const eckit::LocalConfiguration& misc,
                                            const MarsBasedFilterContext& context) noexcept {
    (void)mars;
    (void)misc;
    (void)context;

    return MarsBasedFilterCode::Accepted;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
