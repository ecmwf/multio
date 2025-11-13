/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/Mapper.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/types/LevType.h"
#include "multio/datamod/types/Param.h"
#include "multio/datamod/types/Repres.h"
#include "multio/datamod/types/StatType.h"
#include "multio/datamod/types/TimeDuration.h"


namespace multio::datamod {

using IntOrString = std::variant<std::int64_t, std::string>;

// clang-format off

//----------------------------------------------------------------------------------------------------------------------
// Mars Keys
//----------------------------------------------------------------------------------------------------------------------

/// \defgroup datamod_models_mars
/// \ingroup datamod_models

/// \defgroup datamod_models_mars_keys Mars Keys
/// \ingroup datamod_models_mars
///
/// All MARS keys must support string initialization

/// Global ID keys

constexpr auto ORIGIN =
    EntryDef<IntOrString>{"origin"}
        .withDefault("ecmf")
        .withAccessor([](auto&& v) { return &v.origin; });

constexpr auto CLASS =
    EntryDef<std::string>{"class"}
        .withAccessor([](auto&& v) { return &v.klass; });

constexpr auto STREAM =
    EntryDef<std::string>{"stream"}
        .withAccessor([](auto&& v) { return &v.stream; });

constexpr auto TYPE =
    EntryDef<std::string>{"type"}
        .withAccessor([](auto&& v) { return &v.type; });

constexpr auto EXPVER =
    EntryDef<IntOrString>{"expver"}
        .withAccessor([](auto&& v) { return &v.expver; });

constexpr auto MODEL =
    EntryDef<std::string>{"model"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.model; });


// Field id

constexpr auto PARAM =
    EntryDef<Param>{"param"}
        .withAccessor([](auto&& v) { return &v.param; });

// Horizontal & vertial

constexpr auto LEVTYPE =
    EntryDef<LevType>{"levtype"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.levtype; });

// NOTE: for pressure levels (levtype=pl), this key is in Pa (not hPa) in MultIO
constexpr auto LEVELIST =
    EntryDef<std::int64_t>{"levelist"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.levelist; });

// Time

constexpr auto DATE =
    EntryDef<std::int64_t>{"date"}
        .withAccessor([](auto&& v) { return &v.date; });

constexpr auto TIME =
    EntryDef<std::int64_t>{"time"}
        .withAccessor([](auto&& v) { return &v.time; });

constexpr auto STEP =
    EntryDef<TimeDuration>{"step"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.step; });

constexpr auto TIMESPAN =
    EntryDef<TimeDuration>{"timespan"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.timespan; });

constexpr auto STATTYPE =
    EntryDef<StatType>{"stattype"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.stattype; });

constexpr auto ANOFFSET =
    EntryDef<std::int64_t>{"anoffset"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.anoffset; });

constexpr auto HDATE =
    EntryDef<std::int64_t>{"hdate"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.hdate; });

constexpr auto REFDATE =
    EntryDef<std::int64_t>{"refdate"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.refdate; });

constexpr auto FCMONTH =
    EntryDef<std::int64_t>{"fcmonth"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.fcmonth; });

constexpr auto FCPERIOD =
    EntryDef<std::int64_t>{"fcperiod"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.fcperiod; });


// Ensemble

constexpr auto NUMBER =
    EntryDef<std::int64_t>{"number"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.number; });

// Satellite

constexpr auto IDENT =
    EntryDef<std::int64_t>{"ident"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.ident; });

constexpr auto INSTRUMENT =
    EntryDef<std::int64_t>{"instrument"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.instrument; });

constexpr auto CHANNEL =
    EntryDef<std::int64_t>{"channel"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.channel; });


// Chemical

constexpr auto CHEM =
    EntryDef<std::int64_t>{"chem"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.chem; });

// Aerosol

constexpr auto WAVELENGTH =
    EntryDef<std::int64_t>{"wavelength"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.wavelength; });


// Wave Spectra

constexpr auto DIRECTION =
    EntryDef<std::int64_t>{"direction"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.direction; });

constexpr auto FREQUENCY =
    EntryDef<std::int64_t>{"frequency"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.frequency; });


// Seasonal

constexpr auto METHOD =
    EntryDef<std::int64_t>{"method"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.method; });

constexpr auto SYSTEM =
    EntryDef<std::int64_t>{"system"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.system; });


// Sensitivity forecast

constexpr auto ITERATION =
    EntryDef<std::int64_t>{"iteration"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.iteration; });

constexpr auto DIAGNOSTIC =
    EntryDef<std::int64_t>{"diagnostic"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.diagnostic; });


// DestinE

constexpr auto DATASET =
    EntryDef<std::string>{"dataset"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.dataset; });

constexpr auto RESOLUTION =
    EntryDef<std::string>{"resolution"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.resolution; });

constexpr auto ACTIVITY =
    EntryDef<std::string>{"activity"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.activity; });

constexpr auto EXPERIMENT =
    EntryDef<std::string>{"experiment"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.experiment; });

constexpr auto GENERATION =
    EntryDef<std::int64_t>{"generation"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.generation; });

constexpr auto REALIZATION =
    EntryDef<std::int64_t>{"realization"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.realization; });


// Postproc - output specifiec

constexpr auto PACKING =
    EntryDef<std::string>{"packing"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.packing; });


// Grid

constexpr auto GRID =
    EntryDef<std::string>{"grid"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.grid; });

constexpr auto TRUNCATION =
    EntryDef<std::int64_t>{"truncation"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.truncation; });


// TODO pgeier this key has been modified and is used internally (with the encoder rules...) should not be handled as
// official mars key
constexpr auto REPRES
    = EntryDef<Repres>{"repres"}
          .tagDefaulted()
          .withAccessor([](auto&& v) { return &v.repres; })
          .withDescription(
              "DEPRECATED - internal key that got expanded to support HEALpix and differs from usual values "
              "`repres` describes the type of representation (e.g. gaussian grid, longitude/latitude, spherical "
              "harmonics) "
              "without defining resolution. It can be derived from `grid` and `truncation`. If passed its value is "
              "compared "
              "against the derived value.")
;

// clang-format on

}  // namespace multio::datamod
