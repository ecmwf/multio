/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <cmath>
#include <optional>
#include <sstream>
#include <type_traits>

#include "eckit/exception/Exceptions.h"

#include "multio/message/Metadata.h"
#include "multio/message/SharedMetadata.h"


namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

namespace detail {

/// Metadata keys that carry the missing value of a field (mtg2 and legacy naming)
inline constexpr const char* missingValueKeys[] = {"misc-missingValue", "missingValue"};

/// @return The value rounded to `Precision`, or nothing if the value is not a floating point number or is already
///         exactly representable in `Precision`.
template <typename Precision>
std::optional<double> roundMissingValue(const MetadataValue& value, const char* key) {
    return value.visit([&](const auto& v) -> std::optional<double> {
        using V = std::decay_t<decltype(v)>;
        if constexpr (std::is_floating_point_v<V>) {
            const auto original = static_cast<double>(v);
            const auto rounded = static_cast<double>(static_cast<Precision>(v));

            if (std::isnan(original) || rounded == original) {
                return std::nullopt;
            }
            if (!std::isfinite(rounded)) {
                std::ostringstream oss;
                oss << "Missing value \"" << key << "\"=" << original
                    << " is not representable in the precision of the field values";
                throw eckit::UserError(oss.str(), Here());
            }
            return rounded;
        }
        else {
            return std::nullopt;
        }
    });
}

}  // namespace detail

//----------------------------------------------------------------------------------------------------------------------

/// Round the missing value in the metadata to the precision of the field values.
///
/// The missing value is carried as a double, while the field values may be single precision. A caller filling
/// a single precision field with a missing value that is not exactly representable as float (e.g. -9999.99) stores
/// a different number (-9999.990234375) than it announces in the metadata. All consumers (actions, metkit, ecCodes)
/// identify missing points by exact equality, hence they would not find any. Rounding the announced value to the
/// precision of the field makes both agree. A value rounded to float is exactly representable as double.
///
/// The passed metadata is not modified: if a value needs to be rounded, a private copy is created and returned.
/// Otherwise the returned object shares the metadata it was called with.
///
/// @tparam Precision Precision of the field values (float or double)
/// @param md Metadata of the field
/// @return Metadata with missing values that are exactly representable in `Precision`
/// @throws eckit::UserError if a finite missing value is not representable as finite value in `Precision`
template <typename Precision>
SharedMetadata normaliseMissingValue(SharedMetadata md) {
    static_assert(std::is_floating_point_v<Precision>, "Precision must be float or double");

    for (const char* key : detail::missingValueKeys) {
        const Metadata& ro = md.read();
        const auto search = ro.find(key);
        if (search == ro.end()) {
            continue;
        }
        if (const auto rounded = detail::roundMissingValue<Precision>(search->second, key); rounded) {
            md.acquire();  // Copy on the first modification, the caller's metadata remains untouched
            md.modify().set(key, *rounded);
        }
    }

    return md;
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::message
