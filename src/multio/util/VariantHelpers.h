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

/// @date July 2023

#pragma once

#include <memory>
#include <variant>

#include "TypeTraits.h"
#include "eckit/utils/VariantHelpers.h"

//-----------------------------------------------------------------------------

namespace multio::util {

template <typename T, typename V>
struct GetVariantIndex;

template <typename T, typename... Ts>
struct GetVariantIndex<T, std::variant<Ts...>>
    : std::integral_constant<size_t, std::variant<TypeTag<Ts>...>{TypeTag<T>{}}.index()> {};

//-----------------------------------------------------------------------------

}  // namespace multio::util


//-----------------------------------------------------------------------------
