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
// Interfacing trait. Usage through `TypeParserDumper<ValueType>` at the end of this file through functions
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
// * Conditionally check at compile time if a `ValueType` can be parsed from a given `InputType`:
// HasParse_v<ParseType<ValueType>, InputType>
//
// # How to use for dumping
// * Dumping a field of type `ValueType`: `TypeDumper<ValueType>::dump(...)`
// * Dumping a field of type `ValueType` to a container `Container`: `TypeDumper<ValueType, Container>::dump(...)`
//
// Conditionally check at compile time if a `ValueType` can be parsed from a given
// `InputType`: HasDump_v<DumpType<ValueType>, const ValueType&>
// * Dumping a field through `dumpToAndVisit` when variant types are not handled explicitly: `TypeDumper<ValueType,
// Container>::dumpToAndVisit(visitor, val)`
//
// # Composed TypeParserDumper trait
// * Parsing: `TypeParserDumper<ValueType>::parse(...)`
// * Dumping: `TypeParserDumper<ValueType>::template dumpTo<Container>(...)`
//   Note that the `Container` template argument must always be given here
// * Dumping: `TypeParserDumper<ValueType>::template writeAndWrite<Container>(...)`
//   Note that the `Container` template argument must always be given here
//
// Dumping with the note of a Container is useful to specialize formatting layout for given containers
// E.g. We are setting lowlevel GRIB2 keys (like time unit) that are represented by an enum with integer representation

namespace multio::datamod {

//=============================================================================

template <typename Spec, typename FromType, class = void>
struct HasParse : std::false_type {};

template <typename Spec, typename FromType>
struct HasParse<Spec, FromType, std::void_t<decltype(Spec::parse(std::declval<FromType>()))>> : std::true_type {};

template <typename Spec, typename FromType>
inline constexpr bool HasParse_v = HasParse<Spec, FromType>::value;

/// C++20 concept
// template <typename Spec, typename FromType>
// concept HasParse = requires(FromType from) {
//     { Spec::parse(from) }; // return type optional, we just care it's valid
// };


template <typename Spec, typename FromType, class = void>
struct HasDump : std::false_type {};

template <typename Spec, typename FromType>
struct HasDump<Spec, FromType, std::void_t<decltype(Spec::dump(std::declval<FromType>()))>> : std::true_type {};


template <typename Spec, typename FromType>
inline constexpr bool HasDump_v = HasDump<Spec, FromType>::value;


/// C++20 concept
// template <typename Spec, typename FromType>
// concept HasParse = requires(FromType from) {
//     { Spec::dump(from) }; // return type optional, we just care it's valid
// };



//=============================================================================

// ParseType class - to be specilized by types with a parse function
template <typename Type>
struct ParseType;

// MappedDumper type - to be specilized by types with a dump function.
// Can also be specilized for a container to produce different results for writing to different type of containers
template <typename Type, typename Container = void>
struct DumpType;


// Accessor to parse values for ValueType
// either via ParseType<> specialization or conversion
template <typename ValueType>
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


    // Type defined conversions
    template <typename Val,
              std::enable_if_t<(!std::is_same_v<ValueType, std::decay_t<Val>> && !HasParse_v<ParseType<ValueType>, Val>
                                && std::is_convertible_v<std::decay_t<Val>, ValueType>),
                               bool>
              = true>
    static ValueType parse(Val&& val) {
        return std::forward<Val>(val);
    }
};


// Accessor to map values for ValueType
// either via DumpTypec<> specialization
// to a basic type that is more appropriate for usual containers.
// Note: Variants are explicitly accepted as a mapped type to allow types to
// have multiple representation.
// Thats why the `dumpToAndVisit` can/should be used
template <typename ValueType, typename Container = void>
struct TypeDumper {
    // DumpType<ValueType, Container>::dump is defined and has precedence over DumpType<ValueType, void> and conversion
    template <typename Val,
              std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>> && !std::is_same_v<Container, void>
                                && HasDump_v<DumpType<ValueType, Container>, Val>),
                               bool>
              = true>
    static decltype(auto) dump(Val&& val) {
        return DumpType<ValueType, Container>::dump(std::forward<Val>(val));
    }

    // DumpType<ValueType, void>::dump is defined and has precedence over conversion
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


    // Identity - if nothing is specialized
    template <typename Val, std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>>
                                              && !HasDump_v<DumpType<ValueType, void>, Val>
                                              && !HasDump_v<DumpType<ValueType, Container>, Val>),
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


template <typename ValueType, typename FromType>
inline constexpr bool CanParse_v = HasParse_v<TypeParser<ValueType>, FromType>;


//=============================================================================

}  // namespace multio::datamod
