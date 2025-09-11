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


#include "multio/datamod/core/EntryDef.h"

#include "multio/mars2grib/generated/InferPDT.h"

#include "multio/mars2grib/grib2/DirFreq.h"
#include "multio/mars2grib/grib2/Level.h"
#include "multio/mars2grib/grib2/Satellite.h"
#include "multio/mars2grib/grib2/Time.h"

namespace multio {

namespace dm = multio::datamod;

namespace mars2grib {


struct Grib2Structure {
    // Section 1
    dm::Entry<std::int64_t> tablesVersion;
    dm::Entry<std::int64_t> localTablesVersion;

    // Section 2
    dm::Entry<std::int64_t> setLocalDefinition;
    dm::Entry<std::int64_t> localDefinitionNumber;
    dm::Entry<std::int64_t> destineLocalVersion;

    // Section 3
    dm::Entry<dm::GridType> gridType;

    // Section 4
    dm::Entry<std::int64_t> productDefinitionTemplateNumber;

    // Section 4
    dm::Entry<std::int64_t> dataRepresentationTemplateNumber;

    static constexpr std::string_view record_name_ = "grib2-struture";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::entryDef("tablesVersion", &Grib2Structure::tablesVersion),

        dm::entryDef("localTablesVersion", &Grib2Structure::localTablesVersion).withDefault(0),

        dm::entryDef("setLocalDefinition", &Grib2Structure::setLocalDefinition).tagOptional(),
        dm::entryDef("localDefinitionNumber", &Grib2Structure::localDefinitionNumber).tagOptional(),
        dm::entryDef("destineLocalVersion", &Grib2Structure::destineLocalVersion).tagOptional(),

        dm::entryDef("gridType", &Grib2Structure::gridType),
        dm::entryDef("productDefinitionTemplateNumber", &Grib2Structure::productDefinitionTemplateNumber),
        dm::entryDef("dataRepresentationTemplateNumber", &Grib2Structure::dataRepresentationTemplateNumber));
};


struct Grib2Layout {
    /// Product categorization mechanism
    /// Needed to generate the productDefinitionTemplateNumber
    rules::PDTCat pdtCat;

    Grib2Structure structure;

    grib2::DateTimeKeys dateTime;
    std::optional<grib2::RefDateTimeKeys> refDateTime;

    // Geo Keys not included as passed separately
    std::optional<grib2::InitForecastTimeKeys> initForecastTime;
    std::optional<grib2::PointInTimeKeys> pointInTime;
    std::optional<grib2::TimeRangeKeys> timeRange;

    std::optional<grib2::LevelKeys> level;
    std::optional<grib2::VerticalKeys> vertical;           //> Cached - only exists for a few levels
    std::optional<grib2::DirFreqArrayKeys> dirFreqArrays;  //> Cached
    std::optional<grib2::DirFreqMarsKeys> dirFreqMars;     // Set for each message
    std::optional<grib2::SatelliteKeys> satellite;
};


}  // namespace mars2grib

}  // namespace multio
