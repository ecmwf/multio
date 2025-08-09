/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once


#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/core/Compare.h"
#include "multio/datamod/core/Hash.h"
#include "multio/datamod/core/Print.h"


namespace multio::mars2grib {

namespace dm = multio::datamod;

//-----------------------------------------------------------------------------
// MARS encoder hash keys
//-----------------------------------------------------------------------------

// Note - currently the only keys we do not include are direction and frequency.
// Hence it would be enough to reuse the MarsRecord and just unset these keys.
// However with this approach it is made explicit that there is a further logic happening
// that should be distinguished from the existing MarsRecord.

struct MarsCacheRecord {
    dm::EntryType_t<decltype(dm::ORIGIN)> origin;
    dm::EntryType_t<decltype(dm::CLASS)> klass;
    dm::EntryType_t<decltype(dm::STREAM)> stream;
    dm::EntryType_t<decltype(dm::TYPE)> type;
    dm::EntryType_t<decltype(dm::EXPVER)> expver;
    dm::EntryType_t<decltype(dm::PARAM)> param;
    dm::EntryType_t<decltype(dm::DATE)> date;
    dm::EntryType_t<decltype(dm::TIME)> time;
    dm::EntryType_t<decltype(dm::STEP)> step;
    dm::EntryType_t<decltype(dm::LEVTYPE)> levtype;
    dm::EntryType_t<decltype(dm::LEVELIST)> levelist;
    dm::EntryType_t<decltype(dm::MODEL)> model;
    dm::EntryType_t<decltype(dm::RESOLUTION)> resolution;
    dm::EntryType_t<decltype(dm::ACTIVITY)> activity;
    dm::EntryType_t<decltype(dm::EXPERIMENT)> experiment;
    dm::EntryType_t<decltype(dm::GENERATION)> generation;
    dm::EntryType_t<decltype(dm::REALIZATION)> realization;
    dm::EntryType_t<decltype(dm::TIMESPAN)> timespan;
    dm::EntryType_t<decltype(dm::ANOFFSET)> anoffset;
    dm::EntryType_t<decltype(dm::PACKING)> packing;
    dm::EntryType_t<decltype(dm::NUMBER)> number;
    dm::EntryType_t<decltype(dm::IDENT)> ident;
    dm::EntryType_t<decltype(dm::INSTRUMENT)> instrument;
    dm::EntryType_t<decltype(dm::CHANNEL)> channel;
    dm::EntryType_t<decltype(dm::CHEM)> chem;
    dm::EntryType_t<decltype(dm::WAVELENGTH)> wavelength;
    // The only keys that are completly excluded
    // dm::EntryType_t<decltype(dm::DIRECTION)> direction;
    // dm::EntryType_t<decltype(dm::FREQUENCY)> frequency;
    dm::EntryType_t<decltype(dm::HDATE)> hdate;
    dm::EntryType_t<decltype(dm::DATASET)> dataset;
    dm::EntryType_t<decltype(dm::METHOD)> method;
    dm::EntryType_t<decltype(dm::SYSTEM)> system;
    dm::EntryType_t<decltype(dm::GRID)> grid;
    dm::EntryType_t<decltype(dm::TRUNCATION)> truncation;
    dm::EntryType_t<decltype(dm::REPRES)> repres;

    static constexpr std::string_view record_name_ = "mars";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::ORIGIN, dm::CLASS, dm::STREAM, dm::TYPE, dm::EXPVER, dm::PARAM, dm::DATE, dm::TIME, dm::STEP, dm::LEVTYPE,
        dm::LEVELIST, dm::MODEL, dm::RESOLUTION, dm::ACTIVITY, dm::EXPERIMENT, dm::GENERATION, dm::REALIZATION,
        dm::TIMESPAN, dm::ANOFFSET, dm::PACKING, dm::NUMBER, dm::IDENT, dm::INSTRUMENT, dm::CHANNEL, dm::CHEM,
        dm::WAVELENGTH, dm::HDATE, dm::DATASET, dm::METHOD, dm::SYSTEM, dm::GRID, dm::TRUNCATION, dm::REPRES);
};

}  // namespace multio::mars2grib

namespace multio::datamod {

template <>
struct ApplyRecordDefaults<mars2grib::MarsCacheRecord> {
    static void applyDefaults(mars2grib::MarsCacheRecord& cacheKeys) {

        if (cacheKeys.levtype.has() && cacheKeys.levtype.get() == LevType::ML) {
            cacheKeys.levelist.unset();
        }

        // Explicitly acquire because all the whole data structure is ment to be stored in a container
        acquireRecord(cacheKeys);
    }
};

}  // namespace multio::datamod

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
