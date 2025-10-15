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

#include <cstdint>
#include "multio/datamod/MarsMiscGeo.h"

#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Print.h"

// Level config
namespace multio::mars2grib::grib2 {

namespace dm = multio::datamod;

struct DirFreqArrayKeys {
    dm::Entry<int64_t> numberOfWaveDirections;
    dm::Entry<int64_t> scaleFactorOfWaveDirections;
    dm::Entry<std::vector<int64_t>> scaledValuesOfWaveDirections;

    dm::Entry<int64_t> numberOfWaveFrequencies;
    dm::Entry<int64_t> scaleFactorOfWaveFrequencies;
    dm::Entry<std::vector<int64_t>> scaledValuesOfWaveFrequencies;

    static constexpr std::string_view record_name_ = "dirfreq-array-keys";
    static constexpr auto record_entries_ = std::make_tuple(                                        //
        entryDef("numberOfWaveDirections", &DirFreqArrayKeys::numberOfWaveDirections),              //
        entryDef("scaleFactorOfWaveDirections", &DirFreqArrayKeys::scaleFactorOfWaveDirections),    //
        entryDef("scaledValuesOfWaveDirections", &DirFreqArrayKeys::scaledValuesOfWaveDirections),  //
        entryDef("numberOfWaveFrequencies", &DirFreqArrayKeys::numberOfWaveFrequencies),            //
        entryDef("scaleFactorOfWaveFrequencies", &DirFreqArrayKeys::scaleFactorOfWaveFrequencies),  //
        entryDef("scaledValuesOfWaveFrequencies", &DirFreqArrayKeys::scaledValuesOfWaveFrequencies));
};


/// Set direction and frequences GRIB2 keys.
/// Requires necessary additional keys, throws otherwise
DirFreqArrayKeys setDirFreqArrays(const dm::MiscRecord&);


struct DirFreqMarsKeys {
    dm::Entry<std::int64_t> waveDirectionNumber;
    dm::Entry<std::int64_t> waveFrequencyNumber;

    static constexpr std::string_view record_name_ = "dirfreq-mars-keys";
    static constexpr auto record_entries_ = std::make_tuple(                     //
        entryDef("waveDirectionNumber", &DirFreqMarsKeys::waveDirectionNumber),  //
        entryDef("waveFrequencyNumber", &DirFreqMarsKeys::waveFrequencyNumber));
};


/// Set direction and frequences GRIB2 keys.
/// Requires necessary additional keys, throws otherwise
DirFreqMarsKeys setDirFreqMars(const dm::FullMarsRecord&);

}  // namespace multio::mars2grib::grib2

namespace multio::util {
template <>
struct Print<multio::mars2grib::grib2::DirFreqArrayKeys> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::grib2::DirFreqMarsKeys> : multio::datamod::PrintRecord {};
};  // namespace multio::util

