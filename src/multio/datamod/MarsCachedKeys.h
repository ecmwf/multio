/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once


#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/Compare.h"
#include "multio/datamod/core/Hash.h"
#include "multio/datamod/core/Print.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------
// MARS encoder hash keys
//-----------------------------------------------------------------------------

// Note - currently the only keys we do not include are direction and frequency.
// Hence it would be enough to reuse the FullMarsRecord and just unset these keys.
// However with this approach it is made explicit that there is a further logic happening
// that should be distinguished from the existing FullMarsRecord.

struct MarsCacheRecord : FullMarsRecord {
    static constexpr std::string_view record_name_ = "mars-cache";

    static void applyDefaults(MarsCacheRecord& cacheKeys) {
        if (cacheKeys.levtype.isSet() && cacheKeys.levtype.get() == LevType::ML) {
            cacheKeys.levelist.unset();
        }

        // For the grib structure, it is just important if timespan is set or not - the actual value does not affect the
        // structure Time keys will always get set later
        if (cacheKeys.timespan.isSet()) {
            cacheKeys.timespan.set(0);
        }

        if (cacheKeys.step.isSet()) {
            cacheKeys.step.set(0);
        }
        if (cacheKeys.date.isSet()) {
            cacheKeys.date.set(19700101);
        }
        if (cacheKeys.time.isSet()) {
            cacheKeys.time.set(0);
        }
        cacheKeys.direction.unset();
        cacheKeys.frequency.unset();

        // Explicitly acquire because all the whole data structure is ment to be stored in a container
        acquireRecord(cacheKeys);
    }
};

}  // namespace multio::datamod


namespace std {

template <>
struct equal_to<multio::datamod::MarsCacheRecord> : multio::datamod::EqualToRecord {};
template <>
struct not_equal_to<multio::datamod::MarsCacheRecord> : multio::datamod::NotEqualToRecord {};

template <>
struct hash<multio::datamod::MarsCacheRecord> : multio::datamod::HashRecord {};
}  // namespace std

namespace multio::util {
template <>
struct Print<multio::datamod::MarsCacheRecord> : multio::datamod::PrintRecord {};
}  // namespace multio::util
