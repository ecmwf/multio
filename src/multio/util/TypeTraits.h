#pragma once

#include <memory>  // unique_ptr
#include <optional>
#include <type_traits>
#include <vector>

namespace multio::util {

//-----------------------------------------------------------------------------

template <typename T>
struct TypeTag {};

//-----------------------------------------------------------------------------

template <typename... T>
struct TypeList {
    template <typename TI>
    constexpr static bool contains() {
        return (false || ... || std::is_same_v<T, TI>);
    }
};

template <template <typename...> class Template, typename TypeList_>
struct ApplyTypeList;

template <template <typename...> class Template, typename... T>
struct ApplyTypeList<Template, TypeList<T...>> {
    using type = Template<T...>;
};

template <template <typename...> class Template, typename TypeList_>
using ApplyTypeList_t = typename ApplyTypeList<Template, TypeList_>::type;


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


template <typename Type, typename TypeList>
struct TypeListContains : std::false_type {};

template <typename Type, typename... T>
struct TypeListContains<Type, TypeList<T...>>
    : std::integral_constant<bool, (false || ... || std::is_same_v<Type, T>)> {};

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

template <typename Func>
struct ForwardUnwrappedUniquePtr {
    Func func_;

    template <typename Arg,
              std::enable_if_t<(IsUniquePtr<std::decay_t<Arg>>::value && std::is_lvalue_reference<Arg>::value), bool>
              = true>
    decltype(auto) operator()(Arg&& arg) && noexcept(noexcept(std::move(func_)(*std::forward<Arg>(arg).get()))) {
        return std::move(func_)(*std::forward<Arg>(arg).get());
    }

    template <typename Arg,
              std::enable_if_t<(IsUniquePtr<std::decay_t<Arg>>::value && std::is_rvalue_reference<Arg>::value), bool>
              = true>
    decltype(auto) operator()(Arg&& arg) && noexcept(
        noexcept(std::move(func_)(std::move(*std::forward<Arg>(arg).get())))) {
        return std::move(func_)(std::move(*std::forward<Arg>(arg).get()));
    }

    template <typename Arg, std::enable_if_t<(!IsUniquePtr<std::decay_t<Arg>>::value), bool> = true>
    decltype(auto) operator()(Arg&& arg) && noexcept(noexcept(std::move(func_)(std::forward<Arg>(arg)))) {
        return std::move(func_)(std::forward<Arg>(arg));
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

}  // namespace multio::util


//-----------------------------------------------------------------------------
