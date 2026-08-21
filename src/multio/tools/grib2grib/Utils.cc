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

#include "multio/tools/grib2grib/Utils.h"

#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "eckit/exception/Exceptions.h"

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Convert an option policy to its preferred YAML spelling.
/// @param policy The policy value to render.
/// @return Stable lowercase string representation.
const char* toString(OptionPolicy policy) {
    switch (policy) {
        case OptionPolicy::TryToHandle:
            return "try-to-handle";
        case OptionPolicy::Ignore:
            return "ignore";
    }
    return "unknown-option-policy";
}

/// @brief Parse an option policy from configuration text.
/// @param value Raw configuration value.
/// @return Parsed policy value.
/// @throw eckit exception If the value is not supported.
/// @note The preferred values are `try-to-handle` and `ignore`.
///       A few compatibility aliases are still accepted while the new pipeline
///       is being developed in parallel with legacy code.
OptionPolicy parseOptionPolicy(const std::string& value) {
    if (value == "try-to-handle") {
        return OptionPolicy::TryToHandle;
    }
    if (value == "ignore" || value == "skip" || value == "log-and-ignore") {
        return OptionPolicy::Ignore;
    }

    throw eckit::BadValue("Unsupported option policy: " + value, Here());
}

/// @brief Read a named option as an OptionPolicy.
/// @param options Configuration object to read from.
/// @param key Name of the option key.
/// @param defaultValue Value returned when the key is absent.
/// @return Parsed option value or the provided default.
OptionPolicy getOptionPolicy(const eckit::LocalConfiguration& options, const std::string& key,
                             OptionPolicy defaultValue) {
    if (!options.has(key)) {
        return defaultValue;
    }
    return parseOptionPolicy(options.getString(key));
}

/// @brief Build a compact wall-clock timestamp prefix for logs.
/// @return String of the form `[YYYY-MM-DD HH:MM:SS]: ` in local time.
std::string timestampString() {
    using clock = std::chrono::system_clock;

    const auto now = clock::now();
    const std::time_t t = clock::to_time_t(now);

    std::tm tm{};

    localtime_r(&t, &tm);

    std::ostringstream out;
    out << '[' << std::put_time(&tm, "%Y-%m-%d %H:%M:%S") << "]: ";

    return out.str();
}

/// @brief Return the shared explanation used when errors are trapped intentionally.
/// @return Stable string literal describing the classify-and-continue error policy.
const char* trappedErrorDisclaimer() {
    return "DISCLAIMER: This code is designed to classify errors. All errors are trapped and the code continues.";
}

/// @brief Print the shared trapped-error disclaimer with a timestamp prefix.
///
/// This helper is used by catch-and-continue paths so logs clearly distinguish
/// intentional error trapping from unexpected silent recovery.
void printTrappedErrorDisclaimer() {
    std::cerr << timestampString() << trappedErrorDisclaimer() << std::endl;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
