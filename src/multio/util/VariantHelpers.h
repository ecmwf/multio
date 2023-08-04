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
