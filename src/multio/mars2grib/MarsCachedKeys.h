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


#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/Compare.h"
#include "multio/datamod/core/Hash.h"
#include "multio/datamod/core/Print.h"


namespace multio::mars2grib {

namespace dm = multio::datamod;

//-----------------------------------------------------------------------------
// MARS encoder hash keys
//-----------------------------------------------------------------------------

struct MarsCacheRecord : dm::FullMarsRecord {
    static constexpr std::string_view record_name_ = "mars-cache";

    static void applyDefaults(mars2grib::MarsCacheRecord& cacheKeys) {
        if (cacheKeys.levtype.isSet() && cacheKeys.levtype.get() == dm::LevType::ML) {
            cacheKeys.levelist.unset();
        }

        cacheKeys.step.unset();
        cacheKeys.direction.unset();
        cacheKeys.frequency.unset();

        // Explicitly acquire because all the whole data structure is ment to be stored in a container
        dm::acquireRecord(cacheKeys);
    }
};

}  // namespace multio::mars2grib


namespace std {

template <>
struct equal_to<multio::mars2grib::MarsCacheRecord> : multio::datamod::EqualToRecord {};
template <>
struct not_equal_to<multio::mars2grib::MarsCacheRecord> : multio::datamod::NotEqualToRecord {};

template <>
struct hash<multio::mars2grib::MarsCacheRecord> : multio::datamod::HashRecord {};
}  // namespace std

namespace multio::util {
template <>
struct Print<multio::mars2grib::MarsCacheRecord> : multio::datamod::PrintRecord {};
}  // namespace multio::util
