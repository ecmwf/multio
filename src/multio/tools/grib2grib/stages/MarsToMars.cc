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
/// @brief Standalone `MarsToMars` stage implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/stages/MarsToMars.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/config/LocalConfiguration.h"

#include "metkit/mars2mars/api/Mars2Mars.h"

#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

std::optional<eckit::LocalConfiguration> parseMars2MarsApiOptions(const eckit::LocalConfiguration& config) {
    if (!config.has("api-options")) {
        return std::nullopt;
    }

    if (!config.isSubConfiguration("api-options")) {
        throw eckit::BadValue("mars-to-mars option 'api-options' must be a configuration section", Here());
    }

    return config.getSubConfiguration("api-options");
}

}  // namespace

void validateMarsToMarsContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }

    (void)parseMars2MarsApiOptions(config);
}

MarsToMarsContext parseMarsToMarsContext(const eckit::LocalConfiguration& config) {
    MarsToMarsContext parsed;

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    parsed.apiOptions = parseMars2MarsApiOptions(config);

    return parsed;
}

void freeMarsToMarsContext(MarsToMarsContext& context) noexcept {
    (void)context;
}

MarsToMarsResult runMarsToMarsStage(const eckit::LocalConfiguration& mars, const eckit::LocalConfiguration& misc,
                                    const MarsToMarsContext& context) noexcept {
    using metkit::mars2mars::Mars2Mars;

    (void)context;

    MarsToMarsResult result;

    try {
        auto mars2mars = context.apiOptions ? Mars2Mars(*context.apiOptions) : Mars2Mars();
        const auto marsMisc= mars2mars.convert(mars, misc);
        result.mars = marsMisc.mars;
        result.misc = marsMisc.misc;
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = MarsToMarsCode::MappingsFailed;
        return result;
    }

    result.outcome = MarsToMarsCode::Valid;
    return result;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
