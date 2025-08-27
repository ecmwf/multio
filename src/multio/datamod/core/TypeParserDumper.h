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

#include <type_traits>
#include <utility>
#include "multio/util/VariantHelpers.h"

// TLDR:
// Interfacing trait. Usage through `TypeParserDumper<ValueType, Mapper>` at the end of this file through functions
// `::parse`, `::dump` or `::dumpToAndVisit`.
// Motivation: Use basic std types (string, int, double, vector, ...), variant
// and enums to express key values of everything that will be parse/written to other containers like
// `eckit::LocalConfiguration`/YAML/JSON, Metadata, GRIB etc. There are multiple situations we have to face:
//   * Keys that are represented by one type but can be converted/constructed from other types (i.e. param/paramId)
//   * Keys that are represented by an enum internally but are parse/written as strings or - in case of some GRIB keys -
//   are even written as string and int representation.
//   * Keys that are represented by more complex typse.
//     E.g. a time duration comes with a count and a unit, can be converted from/to integers with a default unit but is
//     fully expressed as string. Yet internally we need to control it properly
//
// More details:
//
// This file describes an interface for customization mapping methods to parse/dump (or encode/decode) for existing
// types through specilization of classes `ParseType<>` or `DumpType<>` with the static methods `parse` and `dump`.
//
// For user defined types, instead of customizing `ParseType<>` appropriate constructors can be added.
// However, for some types (e.g. Enum) it may be convenient to have this external specialization method.
// This is also helpful to extend existing types with additional parsing & validation steps that may blow
// up the type unessesarily.
//
// Sometimes specific keys are representable through a simple type (e.g. an integer or string) and should
// remain being simple, then it may be convenient to create a custom Mapper type that exposes `parse` and `dump`.
//
// # Illustration
//
//   INPUT                      Internal        Output
//
//   TYPE parse(INP_TYPE_A)                      TYPE     dump(TYPE) (default id function)
//        ...             )  =>   TYPE       =>  OUT_TYPE dump(TYPE)
//   TYPE parse(INP_TYPE_X)                      OUT_TYPE dumpTo<Container>(TYPE)
//                                                    (specialization of output type for specific containers)
//
// # How to use for parsing
// * Parsing a field of type `ValueType`: `TypeParserDumper<ValueType>::parse(...)`
// * Parsing a field of type `ValueType` with supplied CustomMapper: `TypeParserDumper<ValueType,
// CustomMapper>::parse(...)`
// * Conditionally check at compile time if a `ValueType` can be parsed from a given `InputType`:
// HasParse_v<ParseType<ValueType>, InputType>
//
// # How to use for dumping
// * Dumping a field of type `ValueType`: `TypeDumper<ValueType>::dump(...)`
// * Dumping a field of type `ValueType` with supplied CustomMapper: `TypeDumper<ValueType, void,
// CustomMapper>::dump(...)`
// * Dumping a field of type `ValueType` to a container `Container`: `TypeDumper<ValueType, Container>::dump(...)`
// * Dumping a field of type `ValueType` to a container `Container` with supplied CustomMapper: `TypeDumper<ValueType,
// Container, CustomMapper>::dump(...)` Conditionally check at compile time if a `ValueType` can be parsed from a given
// `InputType`: HasDump_v<DumpType<ValueType>, const ValueType&>
// * Dumping a field through `dumpToAndVisit` when variant types are not handled explicitly: `TypeDumper<ValueType,
// Container, Mapper>::dumpToAndVisit(visitor, val)`
//
// # Composed TypeParserDumper trait
// * Parsing: `TypeParserDumper<ValueType, Mapper>::parse(...)`
// * Dumping: `TypeParserDumper<ValueType, Mapper>::template dumpTo<Container>(...)`
//   Note that the `Container` template argument must always be given here
// * Dumping: `TypeParserDumper<ValueType, Mapper>::template writeAndWrite<Container>(...)`
//   Note that the `Container` template argument must always be given here
//
// Dumping with the note of a Container is useful to specialize formatting layout for given containers
// E.g. We are setting lowlevel GRIB2 keys (like time unit) that are represented by an enum with integer representation

namespace multio::datamod {

//=============================================================================

template <typename MapperOrSpec, typename FromType, class = void>
struct HasParse : std::false_type {};

template <typename MapperOrSpec, typename FromType>
struct HasParse<MapperOrSpec, FromType, std::void_t<decltype(MapperOrSpec::parse(std::declval<FromType>()))>>
    : std::true_type {};

template <typename MapperOrSpec, typename FromType>
inline constexpr bool HasParse_v = HasParse<MapperOrSpec, FromType>::value;


template <typename MapperOrSpec, typename FromType, class = void>
struct HasDump : std::false_type {};

template <typename MapperOrSpec, typename FromType>
struct HasDump<MapperOrSpec, FromType, std::void_t<decltype(MapperOrSpec::dump(std::declval<FromType>()))>>
    : std::true_type {};


template <typename MapperOrSpec, typename FromType>
inline constexpr bool HasDump_v = HasDump<MapperOrSpec, FromType>::value;


template <typename MapperOrSpec, typename Container, typename FromType, class = void>
struct HasDumpTo : std::false_type {};

template <typename MapperOrSpec, typename Container, typename FromType>
struct HasDumpTo<MapperOrSpec, Container, FromType,
                 std::void_t<decltype(MapperOrSpec::template dumpTo<Container>(std::declval<FromType>()))>>
    : std::true_type {};


template <typename MapperOrSpec, typename Container, typename FromType>
inline constexpr bool HasDumpTo_v = HasDumpTo<MapperOrSpec, Container, FromType>::value;


//=============================================================================

// ParseType class - to be specilized by types with a parse function
template <typename Type>
struct ParseType;

// MappedDumper type - to be specilized by types with a dump function.
// Can also be specilized for a container to produce different results for writing to different type of containers
template <typename Type, typename Container = void>
struct DumpType;


struct DefaultMapper {};

// Accessor to parse values for ValueType
// either via ParseType<> specialization, a CustomMapper::parse
// or conversion
template <typename ValueType, typename CustomMapper = DefaultMapper>
struct TypeParser {
    // Identity
    template <typename Val, std::enable_if_t<std::is_same_v<ValueType, std::decay_t<Val>>, bool> = true>
    static decltype(auto) parse(Val&& val) {
        return std::forward<Val>(val);
    }

    // ParseType is defined
    template <
        typename Val,
        std::enable_if_t<(!std::is_same_v<ValueType, std::decay_t<Val>> && HasParse_v<ParseType<ValueType>, Val>), bool>
        = true>
    static decltype(auto) parse(Val&& val) {
        return ParseType<ValueType>::parse(std::forward<Val>(val));
    }

    // Custom mapper is defined
    template <typename Val,
              std::enable_if_t<(!std::is_same_v<ValueType, std::decay_t<Val>> && !HasParse_v<ParseType<ValueType>, Val>
                                && HasParse_v<CustomMapper, Val>),
                               bool>
              = true>
    static decltype(auto) parse(Val&& val) {
        return CustomMapper::parse(std::forward<Val>(val));
    }


    // Type defined conversions
    template <typename Val,
              std::enable_if_t<(!std::is_same_v<ValueType, std::decay_t<Val>> && !HasParse_v<CustomMapper, Val>
                                && !HasParse_v<ParseType<ValueType>, Val>
                                && std::is_convertible_v<std::decay_t<Val>, ValueType>),
                               bool>
              = true>
    static ValueType parse(Val&& val) {
        return std::forward<Val>(val);
    }
};


// Accessor to map values for ValueType
// either via DumpTypec<> specialization or a CustomMapper::dump
// to a basic type that is more appropriate for usual containers.
// Note: Variants are explicitly accepted as a mapped type to allow types to
// have multiple representation.
// Thats why the `dumpToAndVisit` can/should be used
template <typename ValueType, typename Container = void, typename CustomMapper = DefaultMapper>
struct TypeDumper {
    // DumpType<ValueType, Container>::dump is defined and has precedence over DumpType<ValueType, void>,
    // CustomMapper and conversion
    template <typename Val,
              std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>> && !std::is_same_v<Container, void>
                                && HasDump_v<DumpType<ValueType, Container>, Val>),
                               bool>
              = true>
    static decltype(auto) dump(Val&& val) {
        return DumpType<ValueType, Container>::dump(std::forward<Val>(val));
    }

    // DumpType<ValueType, void>::dump is defined and has precedence over CustomMapper  or conversion
    template <typename Val, std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>>
                                              && (std::is_same_v<Container, void>
                                                  || (!std::is_same_v<Container, void>
                                                      && !HasDump_v<DumpType<ValueType, Container>, Val>))
                                              && HasDump_v<DumpType<ValueType, void>, Val>),
                                             bool>
                            = true>
    static decltype(auto) dump(Val&& val) {
        return DumpType<ValueType, void>::dump(std::forward<Val>(val));
    }


    // CustomMapper::dumpTo<Container> is defined and has precedence over DumpType or conversion
    template <typename Val,
              std::enable_if_t<
                  (std::is_same_v<ValueType, std::decay_t<Val>> && !HasDump_v<DumpType<ValueType, void>, Val>
                   && !HasDump_v<DumpType<ValueType, Container>, Val> && HasDumpTo_v<CustomMapper, Container, Val>),
                  bool>
              = true>
    static decltype(auto) dump(Val&& val) {
        return CustomMapper::template dumpTo<Container>(std::forward<Val>(val));
    }

    // CustomMapper::dump is defined and has precedence over DumpType or conversion
    template <
        typename Val,
        std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>> && !HasDump_v<DumpType<ValueType, void>, Val>
                          && !HasDump_v<DumpType<ValueType, Container>, Val>
                          && !HasDumpTo_v<CustomMapper, Container, Val> && HasDump_v<CustomMapper, Val>),
                         bool>
        = true>
    static decltype(auto) dump(Val&& val) {
        return CustomMapper::dump(std::forward<Val>(val));
    }


    // Identity - if nothing is specialized
    template <
        typename Val,
        std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>> && !HasDump_v<DumpType<ValueType, void>, Val>
                          && !HasDump_v<DumpType<ValueType, Container>, Val>
                          && !HasDumpTo_v<CustomMapper, Container, Val> && !HasDump_v<CustomMapper, Val>),
                         bool>
        = true>
    static decltype(auto) dump(Val&& val) {
        return std::forward<Val>(val);
    }

    template <typename Val, typename Func>
    static decltype(auto) dumpToAndVisit(Val&& val, Func&& func) {
        return util::visitOrForward(std::forward<Func>(func), dump(std::forward<Val>(val)));
    }
};


// Combined accessor parse & and dump
template <typename ValueType, typename Mapper>
struct TypeParserDumper {
    template <typename V>
    inline static constexpr bool CanCreateFromValue_v = HasParse_v<TypeParser<ValueType, Mapper>, V>;

    template <typename Val, std::enable_if_t<CanCreateFromValue_v<Val>, bool> = true>
    static decltype(auto) parse(Val&& val) {
        return TypeParser<ValueType, Mapper>::parse(std::forward<Val>(val));
    }

    template <typename Container, typename Val>
    static decltype(auto) dumpTo(Val&& val) {
        return TypeDumper<ValueType, Container, Mapper>::dump(std::forward<Val>(val));
    }

    template <typename Container, typename Val, typename Func>
    static decltype(auto) dumpToAndVisit(Val&& val, Func&& func) {
        return TypeDumper<ValueType, Container, Mapper>::dumpToAndVisit(std::forward<Val>(val), std::forward<Func>(func));
    }
};


//=============================================================================

}  // namespace multio::datamod
