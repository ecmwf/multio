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

// Level config
namespace multio::mars2grib::grib2 {

namespace dm = multio::datamod;

struct SatelliteKeys {
    dm::Entry<std::int64_t> numberOfContributingSpectralBands;

    dm::Entry<std::int64_t> satelliteSeries;
    dm::Entry<std::int64_t> satelliteNumber;
    dm::Entry<std::int64_t> instrumentType;
    dm::Entry<double> scaleFactorOfCentralWaveNumber;
    dm::Entry<double> scaledValueOfCentralWaveNumber;

    static constexpr std::string_view record_name_ = "satellite-keys";
    static constexpr auto record_entries_ = std::make_tuple(  //
        entryDef("numberOfContributingSpectralBands", &SatelliteKeys::numberOfContributingSpectralBands),
        entryDef("satelliteSeries", &SatelliteKeys::satelliteSeries),
        entryDef("satelliteNumber", &SatelliteKeys::satelliteNumber),
        entryDef("instrumentType", &SatelliteKeys::instrumentType),
        entryDef("scaleFactorOfCentralWaveNumber", &SatelliteKeys::scaleFactorOfCentralWaveNumber),
        entryDef("scaledValueOfCentralWaveNumber", &SatelliteKeys::scaledValueOfCentralWaveNumber));
};


SatelliteKeys setSatellite(const dm::FullMarsRecord&, const dm::MiscRecord&);

}  // namespace multio::mars2grib::grib2
