/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file MarsCacheKey.h
/// @brief Normalized MARS record used as a cache key by the encoder.
///
/// The cache key is a MarsRecord with specific normalizations applied:
///   - If levtype == ML, levelist is cleared (all ML levels have the same GRIB structure)
///   - Time-varying keys (timespan, step, date, time) are set to sentinel values
///   - direction and frequency are cleared (don't affect GRIB structure)
///
/// Provides hash and equality support for use in std::unordered_map.

#pragma once

#include <cstddef>
#include <functional>
#include <optional>
#include <tuple>

#include "multio/datamod/MarsRecord.h"
#include "multio/util/Hash.h"


namespace multio::datamod {

namespace detail {

// Hash helper for std::optional<T>: distinguishes empty from zero-valued
template <typename T>
std::size_t hashOptional(const std::optional<T>& opt) {
    if (opt.has_value()) {
        // Mix with 1 to distinguish "has value that hashes to 0" from "empty"
        return util::hashAppend(std::size_t{1}, std::hash<T>{}(*opt));
    }
    return std::size_t{0};
}

}  // namespace detail


struct MarsCacheKey {
    MarsRecord mars;

    /// Construct from a MarsRecord, applying normalization for caching.
    explicit MarsCacheKey(MarsRecord rec) : mars{std::move(rec)} { normalize(); }

    bool operator==(const MarsCacheKey& other) const {
        return std::apply([&](const auto&... field) { return ((field.get(mars) == field.get(other.mars)) && ...); },
                          MarsRecord::fields_);
    }

    bool operator!=(const MarsCacheKey& other) const { return !(*this == other); }

private:
    void normalize() {
        // For ML, levelist doesn't affect GRIB structure
        if (mars.levtype.has_value() && *mars.levtype == LevType::ML) {
            mars.levelist.reset();
        }

        // Time-varying fields: normalize to sentinels so they don't affect cache lookup
        if (mars.timespan.has_value()) {
            mars.timespan = "0";
        }
        if (mars.step.has_value()) {
            mars.step = 0;
        }
        if (mars.date.has_value()) {
            mars.date = 19700101;
        }
        if (mars.time.has_value()) {
            mars.time = 0;
        }

        // Wave direction/frequency don't affect GRIB structure
        mars.direction.reset();
        mars.frequency.reset();
    }
};


}  // namespace multio::datamod


template <>
struct std::hash<multio::datamod::MarsCacheKey> {
    std::size_t operator()(const multio::datamod::MarsCacheKey& key) const noexcept {
        std::size_t h = 0;
        std::apply(
            [&](const auto&... field) {
                ((h = multio::util::hashAppend(h, multio::datamod::detail::hashOptional(field.get(key.mars)))), ...);
            },
            multio::datamod::MarsRecord::fields_);
        return h;
    }
};

template <>
struct std::equal_to<multio::datamod::MarsCacheKey> {
    bool operator()(const multio::datamod::MarsCacheKey& lhs, const multio::datamod::MarsCacheKey& rhs) const noexcept {
        return lhs == rhs;
    }
};
