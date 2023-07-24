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

#include <memory>  // unique_ptr
#include <optional>
#include <type_traits>
#include <variant>
#include <vector>

namespace multio::util {

//-----------------------------------------------------------------------------

template <typename T>
struct TypeTag { using type = T; };


//-----------------------------------------------------------------------------

template <typename... T>
struct TypeList {
    template <typename TI>
    constexpr static bool contains() {
        return (false || ... || std::is_same_v<T, TI>);
    }
};


//-------------------------------------

template <template <typename...> class Template, typename TypeList_>
struct ApplyTypeList;

template <template <typename...> class Template, typename... T>
struct ApplyTypeList<Template, TypeList<T...>> {
    using type = Template<T...>;
};

template <template <typename...> class Template, typename TypeList_>
using ApplyTypeList_t = typename ApplyTypeList<Template, TypeList_>::type;


//-------------------------------------

template <template <typename, typename...> class Template, typename TypeList_>
struct MapTypeList;

template <template <typename, typename...> class Template, typename... T>
struct MapTypeList<Template, TypeList<T...>> {
    using type = TypeList<Template<T>...>;
};

template <template <typename, typename...> class Template, typename TypeList_>
using MapTypeList_t = typename MapTypeList<Template, TypeList_>::type;


template <typename TypeList1, typename... TypeLists>
struct MergeTypeList;

template <typename TypeList1, typename... TypeLists>
using MergeTypeList_t = typename MergeTypeList<TypeList1, TypeLists...>::type;

template <typename... T1>
struct MergeTypeList<TypeList<T1...>> {
    using type = TypeList<T1...>;
};

template <typename... T1, typename... T2, typename... TS>
struct MergeTypeList<TypeList<T1...>, TypeList<T2...>, TS...> {
    using type = MergeTypeList_t<TypeList<T1..., T2...>, TS...>;
};




//-------------------------------------

template <template <typename> typename Expr, typename TypeList>
struct TypeListAny : std::false_type {};

template <template <typename> typename Expr, typename... T>
struct TypeListAny<Expr, TypeList<T...>> : std::integral_constant<bool, (false || ... || Expr<T>::value)> {};

template <template <typename> typename Expr, typename TypeList>
inline constexpr bool TypeListAny_v = TypeListAny<Expr, TypeList>::value;


template <template <typename> typename Expr, typename TypeList>
struct TypeListAll : std::false_type {};

template <template <typename> typename Expr, typename... T>
struct TypeListAll<Expr, TypeList<T...>> : std::integral_constant<bool, (true && ... && Expr<T>::value)> {};

template <template <typename> typename Expr, typename TypeList>
inline constexpr bool TypeListAll_v = TypeListAll<Expr, TypeList>::value;


template <typename T>
struct TypeListContainsExpr {
    template <typename T2>
    using Expr = std::is_same<T, T2>;
};

template <typename Type, typename TypeList>
using TypeListContains = TypeListAny<TypeListContainsExpr<Type>::template Expr, TypeList>;

template <typename Type, typename TypeList>
inline constexpr bool TypeListContains_v = TypeListContains<Type, TypeList>::value;

//-----------------------------------------------------------------------------

template <typename From, typename To, class = void> struct NotNarrowConstructible: std::false_type {};
// template <typename From, typename To> struct NotNarrowConstructible<From, To, std::void_t<std::result_of_t<TestNotNarrowConstructible<To>(From)>>>: std::true_type {};
template <typename From, typename To> struct NotNarrowConstructible<From, To, std::void_t<decltype(To{std::declval<From>()})>>: std::true_type {};

template <typename From, typename To>
inline constexpr bool NotNarrowConstructible_v = NotNarrowConstructible<From, To>::value;


template <typename TI>
struct BaseOverloadForResolution {
    TypeTag<TI> operator()(TI) const;
};

template <typename TI>
struct IgnoredOverloadForResolution {
    template <typename T, std::enable_if_t<!std::is_same_v<T, T>, bool> = true>
    TypeTag<TI> operator()(T&&) const;
};


template <typename From, typename... TS> struct SaneOverloadResolution;

// Base case - defines an operator() that will never participate in overload resolution
template <typename From> struct SaneOverloadResolution<From> {
    template <typename T, std::enable_if_t<!std::is_same_v<T, T>, bool> = true>
    TypeTag<void> operator()(T&&) const;
};

// Iterating case... add overload only if no narrow conversion is happening 
template <typename From, typename TI, typename... TS>
struct SaneOverloadResolution<From, TI, TS...> : SaneOverloadResolution<From, TS...>, std::conditional_t<NotNarrowConstructible_v<From, TI>, BaseOverloadForResolution<TI>, IgnoredOverloadForResolution<TI>> {
  using SaneOverloadResolution<From, TS...>::operator();
  using std::conditional_t<NotNarrowConstructible_v<From, TI>, BaseOverloadForResolution<TI>, IgnoredOverloadForResolution<TI>>::operator();
};


// specialization to handle void
template <typename From, typename... TS>
struct SaneOverloadResolution<From, void, TS...> : SaneOverloadResolution<From, TS...> {
  using SaneOverloadResolution<From, TS...>::operator();
  TypeTag<void> operator()() const;
};

// specialization to ignore bool conversion
template <typename From, typename... TS>
struct SaneOverloadResolution<From, bool, TS...> : SaneOverloadResolution<From, TS...> {
  using SaneOverloadResolution<From, TS...>::operator();
  template<typename T, std::enable_if_t<std::is_same_v<std::decay_t<T>, bool>, bool> = true>
    TypeTag<bool> operator()(T) const;
};

// specialization to handle TypeList
template <typename From, typename ...TS1, typename... TS2>
struct SaneOverloadResolution<From, TypeList<TS1...>, TS2...> : SaneOverloadResolution<From, TS1..., TS2...> {
  using SaneOverloadResolution<From, TS1..., TS2...>::operator();
};


template <typename From, typename... TS>
using SaneOverloadResolutionResult_t = typename std::result_of_t<SaneOverloadResolution<From, TS...>(From)>::type;



//-----------------------------------------------------------------------------

// Helper for SFINAE
template <typename T, typename TList, typename Ret = void>
using IfTypeOf = std::enable_if_t<TypeListContains<std::decay_t<T>, TList>::value, Ret>;

template <typename T, typename TList, typename Ret = void>
using IfTypeNotOf = std::enable_if_t<!TypeListContains<std::decay_t<T>, TList>::value, Ret>;


//-----------------------------------------------------------------------------

template <typename T>
struct IsUniquePtr {
    static constexpr bool value = false;
};
template <typename T, typename Deleter>
struct IsUniquePtr<std::unique_ptr<T, Deleter>> {
    static constexpr bool value = true;
};


template <typename T>
T& unwrapUniquePtr(std::unique_ptr<T>& arg) noexcept(noexcept(*arg.get())) {
    return *arg.get();
}
template <typename T>
const T& unwrapUniquePtr(const std::unique_ptr<T>& arg) noexcept(noexcept(*arg.get())) {
    return *arg.get();
}
template <typename T>
T&& unwrapUniquePtr(std::unique_ptr<T>&& arg) noexcept(noexcept(std::move(*arg.get()))) {
    return std::move(*arg.get());
}

template <typename Arg, std::enable_if_t<(!IsUniquePtr<std::decay_t<Arg>>::value), bool> = true>
Arg&& unwrapUniquePtr(Arg&& arg) noexcept(noexcept(std::forward<Arg>(arg))) {
    return std::forward<Arg>(arg);
}


template <typename Func>
struct ForwardUnwrappedUniquePtr {
    Func func_;


    template <typename... Args>
    decltype(auto) operator()(Args&&... args) && noexcept(
        noexcept(std::move(func_)(unwrapUniquePtr(std::forward<Args>(args))...))) {
        return std::move(func_)(unwrapUniquePtr(std::forward<Args>(args))...);
    }
};

template <typename Func>
decltype(auto) forwardUnwrappedUniquePtr(Func&& f) noexcept(noexcept(ForwardUnwrappedUniquePtr<Func>{
    std::forward<Func>(f)})) {
    return ForwardUnwrappedUniquePtr<Func>{std::forward<Func>(f)};
}


//-----------------------------------------------------------------------------------------------------------------------------------------


template <typename T>
struct IsVector {
    static constexpr bool value = false;
};
template <typename T, typename Alloc>
struct IsVector<std::vector<T, Alloc>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsVector_v = IsVector<T>::value;

//-----------------------------------------------------------------------------------------------------------------------------------------

template <typename T>
struct IsOptional {
    static constexpr bool value = false;
};
template <typename T>
struct IsOptional<std::optional<T>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsOptional_v = IsOptional<T>::value;


template <typename T>
struct WrapOptional {
    using type = std::optional<T>;
};
template <typename T>
struct WrapOptional<std::optional<T>> {
    using type = std::optional<T>;
};

template <typename T>
using WrapOptional_t = typename WrapOptional<T>::type;


//-----------------------------------------------------------------------------

template <typename T>
struct IsVariant {
    static constexpr bool value = false;
};
template <typename... T>
struct IsVariant<std::variant<T...>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsVariant_v = IsVariant<T>::value;


//-----------------------------------------------------------------------------

template <typename, class = void>
struct HasVariantBaseType : std::false_type {};

template <typename T>
struct HasVariantBaseType<T, std::void_t<typename T::Base>>
    : std::integral_constant<bool, (IsVariant_v<typename T::Base> && std::is_base_of_v<typename T::Base, T>)> {};

template <typename T>
inline constexpr bool HasVariantBaseType_v = HasVariantBaseType<T>::value;

//-----------------------------------------------------------------------------

}  // namespace multio::util


//-----------------------------------------------------------------------------
