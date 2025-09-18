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

#include <iostream>
#include <variant>

namespace multio::util {

//-----------------------------------------------------------------------------

// TLDR: Alternative mechanism to `operator<<(std::ostream&, ...)` to be used in templated contexts
//
// This is an alternative to rely on implementing `operator<<` to provide printing mechanisms for
// templated types where the nested type also needs to be passed to `operator<<`.
// While CLang is doing this all fine, gcc can complain that it does not find a proper overlead.
// With explicit template instantiation this problem is avoided.
// Moreover it supports indendation.
//
// Possible further option: short or multiline mode (such that implementations can vary)

template <typename T>
struct Print;


// until we can use C++20 ...

// // Concept: checks if Print<T> exists and has a proper print method
// template<typename T>
// concept Printable =
//     is_complete<Print<T>>::value &&  // 1️⃣ must be specialized
//         requires(std::ostream& os, const T& t) {
//                 { Print<T>::print(os, t) } -> std::same_as<void>; // 2️⃣ must have correct signature
//         };
// Or just
// template <typename T>
// concept Printable = requires(PrintStream& ps, const T& value) {
//     { Print<T>::print(ps, value) } -> std::same_as<void>;
// };


// Forward declaration - wrapper around ostream
class PrintStream;


template <typename T, class = void>
struct Printable : std::false_type {};

template <typename T>
struct Printable<T, std::void_t<decltype(Print<T>::print(std::declval<PrintStream&>(), std::declval<const T&>()))>>
    : std::true_type {};

template <typename T>
inline constexpr bool Printable_v = Printable<T>::value;




template <typename T, class = void>
struct OstreamPrintable : std::false_type {};

template <typename T>
struct OstreamPrintable<T, std::void_t<decltype(std::declval<std::ostream&>() << std::declval<const T&>())>>
    : std::true_type {};

template <typename T>
inline constexpr bool OstreamPrintable_v = OstreamPrintable<T>::value;


/// C++20 concept
// template <typename T>
// concept OstreamPrintable = requires(std::ostream& os, const T& value) {
//     { os << value } -> std::same_as<std::ostream&>;
// };


//-----------------------------------------------------------------------------


template <typename T, std::enable_if_t<Printable_v<T>, bool> = true>
void print(PrintStream& ps, const T& v) {
    Print<T>::print(ps, v);
}

// Function to call ostream<< if Print<T>::print is not defined
template <typename T, std::enable_if_t<!Printable_v<T> && OstreamPrintable_v<T>, bool> = true>
void print(PrintStream& ps, const T& v) {
    ps << v;
}
// Function to call ostream<< if Print<T>::print is not defined
template <typename T, std::enable_if_t<!Printable_v<T> && OstreamPrintable_v<T>, bool> = true>
void print(std::ostream& os, const T& v) {
    os << v;
}


// TODO remove - is not hooking up properly in overload resolution
// This enforces that every class that implements Print, must not implement
// operator<<
template <typename T, std::enable_if_t<multio::util::Printable_v<T>, bool> = true>
std::ostream& operator<<(std::ostream& os, const T& v) {
    multio::util::print(os, v);
    return os;
}


//-----------------------------------------------------------------------------

enum class PrintRepres : int64_t
{
    Long,
    Compact
};

class PrintStream {
public:
    explicit PrintStream(std::ostream& os, int indentSize = 2, PrintRepres repres = PrintRepres::Long) :
        os_(os), indentSize_(indentSize), currentIndent_(0), atLineStart_(true), repres_{repres} {}

    void indent() { ++currentIndent_; }
    void unindent() {
        if (currentIndent_ > 0)
            --currentIndent_;
    }

    int indentLevel() const { return currentIndent_; }
    int indentSize() const { return indentSize_; }

    void repres(PrintRepres repres) { repres_ = repres; }
    PrintRepres repres() const { return repres_; }

    /// Adds a new line in long mode and does nothing in soft mode
    void softBreak() {
        if (repres_ == PrintRepres::Long) {
            *this << std::endl;
        }
    }

    template <typename T, std::enable_if_t<Printable_v<T>, bool> = true>
    PrintStream& operator<<(const T& value) {
        writeIndentIfNeeded();
        util::print(*this, value);
        return *this;
    }

    template <typename T, std::enable_if_t<!Printable_v<T> && OstreamPrintable_v<T>, bool> = true>
    PrintStream& operator<<(const T& value) {
        writeIndentIfNeeded();
        os_ << value;
        return *this;
    }

    // Special case for manipulators like std::endl
    PrintStream& operator<<(std::ostream& (*manip)(std::ostream&)) {
        manip(os_);
        atLineStart_ = true;  // next item will be at start of line
        return *this;
    }

private:
    void writeIndentIfNeeded() {
        if (atLineStart_) {
            for (int i = 0; i < currentIndent_ * indentSize_; ++i) {
                os_.put(' ');
            }
            atLineStart_ = false;
        }
    }

    std::ostream& os_;
    int indentSize_;
    int currentIndent_;
    bool atLineStart_;

    PrintRepres repres_;
};


// RAII guard for indentation
class IndentGuard {
public:
    explicit IndentGuard(PrintStream& os) : os_(os) { os_.indent(); }
    ~IndentGuard() { os_.unindent(); }

private:
    PrintStream& os_;
};


// RAII guard for indentation
class PrintRepresGuard {
public:
    explicit PrintRepresGuard(PrintStream& os, PrintRepres repres) : os_(os), repres_{os.repres()} {
        os_.repres(repres);
    }
    ~PrintRepresGuard() { os_.repres(repres_); }

private:
    PrintStream& os_;
    PrintRepres repres_;
};


//-----------------------------------------------------------------------------

// Function to call print on Print<T>
template <typename T, std::enable_if_t<Printable_v<T>, bool> = true>
void print(std::ostream& os, const T& v) {
    PrintStream ps(os);
    Print<T>::print(ps, v);
}


template <typename... T>
struct Print<std::variant<T...>> {
    static void print(PrintStream& os, const std::variant<T...>& var) {
        std::visit([&](const auto& val) { os << val; }, var);
    }
};

//-----------------------------------------------------------------------------


}  // namespace multio::util

