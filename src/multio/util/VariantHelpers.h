
#pragma once

// Helper type for the visitor
template <typename... Ts>
struct Overloaded : Ts... {
    using Ts::operator()...;
};

// Explicit deduction guide (not needed as of C++20)
template <typename... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;
