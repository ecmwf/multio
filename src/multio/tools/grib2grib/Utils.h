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
/// @brief Small generic utilities shared by the new isolated `grib2grib` pipeline.
///
/// This header intentionally stays small. Its purpose is to provide only the
/// generic option-policy datatype and a few generic parsing helpers that can be
/// reused by multiple stages without pulling stage-specific logic into a common
/// utility module.

#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Generic per-stage policy value.
///
/// Each stage is free to interpret the policy according to its own semantics,
/// but the current intended meaning is:
/// - `TryToHandle`: allow the message to continue to the next step
/// - `Ignore`: reject the message intentionally at the current stage
enum class OptionPolicy {
    TryToHandle = 0,
    Ignore,
};

/// @brief Convert an option policy to its preferred YAML spelling.
/// @param policy The policy value to render.
/// @return Stable lowercase string representation.
const char* toString(OptionPolicy);

/// @brief Parse an option policy from configuration text.
/// @param value Raw configuration value.
/// @return Parsed policy value.
/// @throw eckit exception If the value is not supported.
/// @note The preferred values are `try-to-handle` and `ignore`.
///       A few compatibility aliases are still accepted while the new pipeline
///       is being developed in parallel with legacy code.
OptionPolicy parseOptionPolicy(const std::string& value);

/// @brief Read a named option as an OptionPolicy.
/// @param options Configuration object to read from.
/// @param key Name of the option key.
/// @param defaultValue Value returned when the key is absent.
/// @return Parsed option value or the provided default.
OptionPolicy getOptionPolicy(const eckit::LocalConfiguration& options, const std::string& key,
                             OptionPolicy defaultValue);

std::string timestampString();

const char* trappedErrorDisclaimer();
void printTrappedErrorDisclaimer();

}  // namespace multio::distGrib1ToGrib2::grib2grib
