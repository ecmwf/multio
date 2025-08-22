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

#include "multio/datamod/MarsTypes.h"
#include "multio/datamod/core/EntryDef.h"


namespace multio::datamod {

using mapper::StringToIntMapper;

//-----------------------------------------------------------------------------
// Mars Keys
//-----------------------------------------------------------------------------

// All MARS keys must support string initialization

constexpr auto ORIGIN =              //
    EntryDef<IntOrString>{"origin"}  //
        .withDefault("ecmf")
        .withAccessor([](auto&& v) { return &v.origin; });

constexpr auto CLASS =              //
    EntryDef<std::string>{"class"}  //
        .withAccessor([](auto&& v) { return &v.klass; });

constexpr auto STREAM =              //
    EntryDef<std::string>{"stream"}  //
        .withAccessor([](auto&& v) { return &v.stream; });

constexpr auto TYPE =              //
    EntryDef<std::string>{"type"}  //
        .withAccessor([](auto&& v) { return &v.type; });

constexpr auto EXPVER =              //
    EntryDef<IntOrString>{"expver"}  //
        .withAccessor([](auto&& v) { return &v.expver; });

constexpr auto PARAM =                                    //
    EntryDef<std::int64_t, mapper::ParamMapper>{"param"}  //
        .withAccessor([](auto&& v) { return &v.param; });

constexpr auto DATE =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"date"}  //
        .withAccessor([](auto&& v) { return &v.date; });

constexpr auto TIME =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"time"}  //
        .withAccessor([](auto&& v) { return &v.time; });

constexpr auto STEP =               //
    EntryDef<TimeDuration>{"step"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.step; });

constexpr auto LEVTYPE =          //
    EntryDef<LevType>{"levtype"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.levtype; });

constexpr auto LEVELIST =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"levelist"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.levelist; });

constexpr auto MODEL =              //
    EntryDef<std::string>{"model"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.model; });

constexpr auto RESOLUTION =              //
    EntryDef<std::string>{"resolution"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.resolution; });

constexpr auto ACTIVITY =              //
    EntryDef<std::string>{"activity"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.activity; });

constexpr auto EXPERIMENT =              //
    EntryDef<std::string>{"experiment"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.experiment; });

constexpr auto GENERATION =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"generation"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.generation; });

constexpr auto REALIZATION =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"realization"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.realization; });

constexpr auto TIMESPAN =               //
    EntryDef<TimeDuration>{"timespan"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.timespan; });

constexpr auto ANOFFSET =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"anoffset"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.anoffset; });

constexpr auto PACKING =              //
    EntryDef<std::string>{"packing"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.packing; });

constexpr auto NUMBER =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"number"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.number; });

constexpr auto IDENT =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"ident"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.ident; });

constexpr auto INSTRUMENT =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"instrument"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.instrument; });

constexpr auto CHANNEL =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"channel"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.channel; });

constexpr auto CHEM =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"chem"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.chem; });

constexpr auto WAVELENGTH =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"wavelength"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.wavelength; });

constexpr auto DIRECTION =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"direction"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.direction; });

constexpr auto FREQUENCY =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"frequency"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.frequency; });

constexpr auto HDATE =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"hdate"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.hdate; });

constexpr auto DATASET =              //
    EntryDef<std::string>{"dataset"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.dataset; });

constexpr auto METHOD =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"method"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.method; });

constexpr auto SYSTEM =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"system"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.system; });

constexpr auto GRID =              //
    EntryDef<std::string>{"grid"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.grid; });

constexpr auto TRUNCATION =                                  //
    EntryDef<std::int64_t, StringToIntMapper>{"truncation"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.truncation; });


// TODO this key has been modified and is used internally (with the encoder rules...) should not be handled as
// official mars key
constexpr auto REPRES
    = EntryDef<Repres>{"repres"}  //
          .tagDefaulted()
          .withAccessor([](auto&& v) { return &v.repres; })
          .withDescription(
              "DEPRECATED - internal key that got expanded to support HEALpix and differs from usual values "
              "`repres` describes the type of representation (e.g. gaussian grid, longitude/latitude, spherical "
              "harmonics) "
              "without defining resolution. It can be derived from `grid` and `truncation`. If passed its value is "
              "compared "
              "against the derived value.");


//-----------------------------------------------------------------------------

}  // namespace multio::datamod

