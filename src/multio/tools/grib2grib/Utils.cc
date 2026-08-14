/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/grib2grib/Utils.h"

#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "eckit/exception/Exceptions.h"

namespace multio::distGrib1ToGrib2::grib2grib {

const char* toString(OptionPolicy policy) {
    switch (policy) {
        case OptionPolicy::TryToHandle:
            return "try-to-handle";
        case OptionPolicy::Ignore:
            return "ignore";
    }
    return "unknown-option-policy";
}

OptionPolicy parseOptionPolicy(const std::string& value) {
    if (value == "try-to-handle") {
        return OptionPolicy::TryToHandle;
    }
    if (value == "ignore" || value == "skip" || value == "log-and-ignore") {
        return OptionPolicy::Ignore;
    }

    throw eckit::BadValue("Unsupported option policy: " + value, Here());
}

OptionPolicy getOptionPolicy(const eckit::LocalConfiguration& options, const std::string& key,
                             OptionPolicy defaultValue) {
    if (!options.has(key)) {
        return defaultValue;
    }
    return parseOptionPolicy(options.getString(key));
}

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

const char* trappedErrorDisclaimer() {
    return "DISCLAIMER: This code is designed to classify errors. All errors are trapped and the code continues.";
}

void printTrappedErrorDisclaimer() {
    std::cerr << timestampString() << trappedErrorDisclaimer() << std::endl;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
