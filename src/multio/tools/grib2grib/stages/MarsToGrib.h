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
/// @brief Standalone `MarsToGrib` stage for the isolated `grib2grib` pipeline.

#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/StageOutcomes.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::distGrib1ToGrib2::grib2grib {

class TestCaseFileSink;

/// @brief Parsed runtime context for the standalone `MarsToGrib` stage.
struct MarsToGribContext {
    std::int64_t verbosity = 0;
    bool generateTestcases = false;
    std::optional<std::string> testcasesDir;
    std::optional<eckit::LocalConfiguration> apiOptions;
};

/// @brief Result of the standalone `MarsToGrib` stage.
struct MarsToGribResult {
    MarsToGribCode outcome = MarsToGribCode::UnknownFailure;
    std::unique_ptr<metkit::codes::CodesHandle> encoded;
    bool testCaseGenerationFailed = false;
    bool testCaseWriteFailed = false;
};

/// @brief Validate the raw context consumed by the `MarsToGrib` stage.
/// @param config Stage-local context subconfiguration.
/// @throw eckit exception If a known option value is invalid.
void validateMarsToGribContext(const eckit::LocalConfiguration& config);

/// @brief Parse the stage-local `MarsToGrib` context once for reuse.
/// @param config Stage-local context subconfiguration.
/// @return Parsed stage-local runtime context.
MarsToGribContext parseMarsToGribContext(const eckit::LocalConfiguration& config);

void freeMarsToGribContext(MarsToGribContext& context) noexcept;

/// @brief Encode one field into GRIB2 and optionally append a testcase line.
/// @param values Field values to encode.
/// @param mars Input MARS dictionary.
/// @param misc Input misc dictionary.
/// @param context Parsed stage-local runtime context.
/// @param testCaseSink Rank-local testcase sink, or `nullptr` to skip testcase generation.
/// @return Stage outcome and, on success, the encoded GRIB2 message.
MarsToGribResult runMarsToGribStage(const std::vector<double>& values, const eckit::LocalConfiguration& mars,
                                    const eckit::LocalConfiguration& misc, const MarsToGribContext& context,
                                    TestCaseFileSink* testCaseSink) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib
