/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date March 2025

#pragma once

#include <type_traits>
#include <utility>

// This file describes an interface for customization mapping methods to read/write (or encode/decode) for existing types
// through specilization of classes `ReadSpec<>` or `WriteSpec<>` with the static methods `read` and `write`.
//
// For user defined types, instead of customizing `ReadSpec<>` appropriate constructors can be added.
// However, for some types (e.g. Enum) it may be convenient to have this external specialization method.
// This is also helpful to extend existing types with additional parsing & validation steps that may blow
// up the type unessesarily.
//
// Sometimes specific keys are representable through a simple type (e.g. an integer or string) and should
// remain being simple, then it may be convenient to create a custom Mapper type that exposes `read` and `write`.
//
// # How to use for reading
// Parsing a field of type `ValueType`: `Reader<ValueType>`::read(...)
// Parsing a field of type `ValueType` with supplied CustomMapper: `Reader<ValueType, CustomMapper>`::read(...)
// Conditionally check at compile time if a `ValueType` can be parsed from a given `InputType`:
// HasRead_v<Reader<ValueType>, InputType>
//
// # How to use for writing
// Dumping a field of type `ValueType`: `Writer<ValueType>`::write(...)
// Dumping a field of type `ValueType` with supplied CustomMapper: `Writer<ValueType, void, CustomMapper>`::write(...)
// Dumping a field of type `ValueType` to a container `Container`: `Writer<ValueType, Container>`::write(...)
// Dumping a field of type `ValueType` to a container `Container` with supplied CustomMapper: `Writer<ValueType,
// Container, CustomMapper>`::write(...) Conditionally check at compile time if a `ValueType` can be parsed from a given
// `InputType`: HasWrite_v<Writer<ValueType>, const ValueType&>
//
// Dumping with the note of a Container is useful to specialize formatting layout for given containers

namespace multio::datamod {

//=============================================================================

template <typename MapperOrSpec, typename FromType, class = void>
struct HasRead : std::false_type {};

template <typename MapperOrSpec, typename FromType>
struct HasRead<MapperOrSpec, FromType, std::void_t<decltype(MapperOrSpec::read(std::declval<FromType>()))>>
    : std::true_type {};

template <typename MapperOrSpec, typename FromType>
inline constexpr bool HasRead_v = HasRead<MapperOrSpec, FromType>::value;


template <typename MapperOrSpec, typename FromType, class = void>
struct HasWrite : std::false_type {};

template <typename MapperOrSpec, typename FromType>
struct HasWrite<MapperOrSpec, FromType, std::void_t<decltype(MapperOrSpec::write(std::declval<FromType>()))>>
    : std::true_type {};


template <typename MapperOrSpec, typename FromType>
inline constexpr bool HasWrite_v = HasWrite<MapperOrSpec, FromType>::value;


//=============================================================================

// Reader class - to be specilized by types with a read function
template <typename Type>
struct ReadSpec;

// MappedWriter type - to be specilized by types with a write function.
// Can also be specilized for a container to produce different results for writing to different type of containers
template <typename Type, typename Container = void>
struct WriteSpec;


struct DefaultMapper {};

template <typename ValueType, typename CustomMapper = DefaultMapper>
struct Reader {
    // Identity
    template <typename Val, std::enable_if_t<std::is_same_v<ValueType, std::decay_t<Val>>, bool> = true>
    static decltype(auto) read(Val&& val) {
        return std::forward<Val>(val);
    }

    // ReadSpec is defined
    template <
        typename Val,
        std::enable_if_t<(!std::is_same_v<ValueType, std::decay_t<Val>> && HasRead_v<ReadSpec<ValueType>, Val>), bool>
        = true>
    static decltype(auto) read(Val&& val) {
        return ReadSpec<ValueType>::read(std::forward<Val>(val));
    }

    // Custom mapper is defined
    template <typename Val, std::enable_if_t<(!std::is_same_v<ValueType, std::decay_t<Val>>
                                              && !HasRead_v<ReadSpec<ValueType>, Val> && HasRead_v<CustomMapper, Val>),
                                             bool>
                            = true>
    static decltype(auto) read(Val&& val) {
        return CustomMapper::read(std::forward<Val>(val));
    }


    // Type defined conversions
    template <typename Val, std::enable_if_t<(!std::is_same_v<ValueType, std::decay_t<Val>>
                                              && !HasRead_v<CustomMapper, Val> && !HasRead_v<ReadSpec<ValueType>, Val>
                                              && std::is_convertible_v<std::decay_t<Val>, ValueType>),
                                             bool>
                            = true>
    static ValueType read(Val&& val) {
        return std::forward<Val>(val);
    }

    static const ValueType& write(const ValueType& val) { return val; }
};


template <typename ValueType, typename Container = void, typename CustomMapper = DefaultMapper>
struct Writer {
    // WriteSpec<ValueType, Container>::write is defined and has precedence over WriteSpec<ValueType, void>,
    // CustomMapper and conversion
    template <typename Val,
              std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>> && !std::is_same_v<Container, void>
                                && HasWrite_v<WriteSpec<ValueType, Container>, Val>),
                               bool>
              = true>
    static decltype(auto) write(Val&& val) {
        return WriteSpec<ValueType, Container>::write(std::forward<Val>(val));
    }

    // WriteSpec<ValueType, void>::write is defined and has precedence over CustomMapper  or conversion
    template <typename Val, std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>>
                                              && (std::is_same_v<Container, void>
                                                  || (!std::is_same_v<Container, void>
                                                      && !HasWrite_v<WriteSpec<ValueType, Container>, Val>))
                                              && HasWrite_v<WriteSpec<ValueType, void>, Val>),
                                             bool>
                            = true>
    static decltype(auto) write(Val&& val) {
        return WriteSpec<ValueType, void>::write(std::forward<Val>(val));
    }


    // CustomMapper::write is defined and has precedence over WriteSpec or conversion
    template <
        typename Val,
        std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>> && !HasWrite_v<WriteSpec<ValueType, void>, Val>
                          && !HasWrite_v<WriteSpec<ValueType, Container>, Val> && HasWrite_v<CustomMapper, Val>),
                         bool>
        = true>
    static decltype(auto) write(Val&& val) {
        return CustomMapper::write(std::forward<Val>(val));
    }


    // Identity - if nothing is specialized
    template <
        typename Val,
        std::enable_if_t<(std::is_same_v<ValueType, std::decay_t<Val>> && !HasWrite_v<WriteSpec<ValueType, void>, Val>
                          && !HasWrite_v<WriteSpec<ValueType, Container>, Val> && !HasWrite_v<CustomMapper, Val>),
                         bool>
        = true>
    static decltype(auto) write(Val&& val) {
        return std::forward<Val>(val);
    }
};


//=============================================================================

}  // namespace multio::datamod
