
#pragma once

template <typename>
inline constexpr bool always_false_v = false;

// Helper type for the visitor
template <typename... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};

// Explicit deduction guide (not needed as of C++20)
template <typename... Ts>
overloaded(Ts...) -> overloaded<Ts...>;
