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

#include "eckit/exception/Exceptions.h"

#include "multio/message/Metadata.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

std::optional<multio::message::match::MatchReduce> parseSelectors(const eckit::LocalConfiguration& config) {
    if (!config.has("selectors")) {
        return std::nullopt;
    }

    if (!config.isSubConfiguration("selectors")) {
        throw eckit::BadValue("mars-based-filter option 'selectors' must be a configuration section", Here());
    }

    return multio::message::match::MatchReduce::construct(config.getSubConfiguration("selectors"));
}

multio::message::Metadata buildMetadata(const eckit::LocalConfiguration& mars, const eckit::LocalConfiguration& misc) {
    multio::message::Metadata metadata = multio::message::toMetadata(mars);
    metadata.updateOverwrite(multio::message::toMetadata(misc));
    return metadata;
}

}  // namespace

void validateMarsBasedFilterContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }

    (void)parseSelectors(config);
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

    parsed.selectors = parseSelectors(config);

    return parsed;
}

void freeMarsBasedFilterContext(MarsBasedFilterContext& context) noexcept {
    (void)context;
}

MarsBasedFilterCode runMarsBasedFilterStage(const eckit::LocalConfiguration& mars,
                                            const eckit::LocalConfiguration& misc,
                                            const MarsBasedFilterContext& context) noexcept {
    if (!context.selectors) {
        return MarsBasedFilterCode::Accepted;
    }

    try {
        const multio::message::Metadata metadata = buildMetadata(mars, misc);
        return context.selectors->matches(metadata) ? MarsBasedFilterCode::Rejected : MarsBasedFilterCode::Accepted;
    }
    catch (...) {
        return MarsBasedFilterCode::Rejected;
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
