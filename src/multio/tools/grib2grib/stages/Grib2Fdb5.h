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
/// @brief Standalone `Grib2Fdb5` stage for the isolated `grib2grib` pipeline.

#pragma once

#include <cstdint>

#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/StageOutcomes.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::sink {
class DataSink;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Parsed runtime context for the standalone `Grib2Fdb5` stage.
struct Grib2Fdb5Context {
    std::int64_t verbosity = 0;
};

/// @brief Result of the standalone `Grib2Fdb5` stage.
struct Grib2Fdb5Result {
    Grib2Fdb5Code outcome = Grib2Fdb5Code::UnknownFailure;
};

/// @brief Validate the raw context consumed by the `Grib2Fdb5` stage.
/// @param config Stage-local context subconfiguration.
/// @throw eckit exception If a known option value is invalid.
void validateGrib2Fdb5Context(const eckit::LocalConfiguration& config);

/// @brief Parse the stage-local `Grib2Fdb5` context once for reuse.
/// @param config Stage-local context subconfiguration.
/// @return Parsed stage-local runtime context.
Grib2Fdb5Context parseGrib2Fdb5Context(const eckit::LocalConfiguration& config);

void freeGrib2Fdb5Context(Grib2Fdb5Context& context) noexcept;

/// @brief Write one encoded GRIB2 message to the sink writer.
/// @param encodedHandle Encoded GRIB2 handle.
/// @param context Parsed stage-local runtime context.
/// @param writer Rank-local sink writer.
/// @return Stage outcome.
Grib2Fdb5Result runGrib2Fdb5Stage(const metkit::codes::CodesHandle& encodedHandle, const Grib2Fdb5Context& context,
                                  multio::sink::DataSink& writer) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib
