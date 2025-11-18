/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/Print.h"
#include "multio/util/Print.h"
#include "multio/util/TypeToString.h"

#include <functional>
#include <iostream>
#include <tuple>
#include <vector>


namespace multio::mars2grib::matcher {

namespace dm = multio::datamod;

// Matchers are defined on a key that serves as accessor and transports type information ... this saves defining a lot
// functions.
// Alternatively they could be defined for a given value type.

// Range of values to match
template <typename EntryDef_>
struct Range {
    std::reference_wrapper<const EntryDef_> entryDef;
    dm::EntryValueType_t<EntryDef_> first;
    dm::EntryValueType_t<EntryDef_> last;

    bool operator()(const dm::FullMarsRecord& rec) const {
        const auto& entry = entryDef.get().get(rec);

        return (entry.isSet() && ((entry.get() >= first) && (entry.get() <= last)));
    }
};

template <typename EntryDef_, typename... Args>
Range(const EntryDef_&, Args&&...) -> Range<EntryDef_>;


// List of ranges
template <typename EntryDef_>
struct Ranges {
    std::vector<Range<EntryDef_>> ranges;

    bool operator()(const dm::FullMarsRecord& rec) const {
        for (const auto& range : ranges) {
            if (range(rec)) {
                return true;
            }
        }
        return false;
    }
};
template <typename EntryDef_, typename... Args>
Ranges(const EntryDef_&, Args&&...) -> Ranges<EntryDef_>;


// Check if the value of a field matches any of the set of values given
// Returns false if the field is not given
template <typename EntryDef_>
struct OneOf {
    // Using a vector because the only operation we perform is search, mostly on a few elements.
    // Having contiguous memory access is helpful here
    std::reference_wrapper<const EntryDef_> entryDef;
    std::vector<dm::EntryValueType_t<EntryDef_>> values;

    bool operator()(const dm::FullMarsRecord& rec) const {
        const auto& entry = entryDef.get().get(rec);

        return (entry.isSet() && (std::find(values.begin(), values.end(), entry.get()) != values.end()));
    }
};

template <typename EntryDef_>
OneOf(const EntryDef_&, std::vector<dm::EntryValueType_t<EntryDef_>>) -> OneOf<EntryDef_>;


// Check if the value of a field is not in a given exclusion list
// Returns false if the field is not given
template <typename EntryDef_>
struct NoneOf {
    std::reference_wrapper<const EntryDef_> entryDef;
    std::vector<dm::EntryValueType_t<EntryDef_>> values;

    bool operator()(const dm::FullMarsRecord& rec) const {
        const auto& entry = entryDef.get().get(rec);

        return !entry.isSet() || (std::find(values.begin(), values.end(), entry.get()) == values.end());
    }
};
template <typename EntryDef_>
NoneOf(const EntryDef_&, std::vector<dm::EntryValueType_t<EntryDef_>>) -> NoneOf<EntryDef_>;


// Checks if a field is given
template <typename EntryDef_>
struct Has {
    std::reference_wrapper<const EntryDef_> entryDef;

    bool operator()(const dm::FullMarsRecord& rec) const { return entryDef.get().get(rec).isSet(); }
};
template <typename EntryDef_>
Has(const EntryDef_&) -> Has<EntryDef_>;


// Checks if a field is missing
template <typename EntryDef_>
struct Missing {
    std::reference_wrapper<const EntryDef_> entryDef;

    bool operator()(const dm::FullMarsRecord& rec) const { return !entryDef.get().get(rec).isSet(); }
};
template <typename EntryDef_>
Missing(const EntryDef_&) -> Missing<EntryDef_>;


// Match a binary operation like >, >=, <, <=
template <typename EntryDef_, typename OpFunctor>
struct MatchOp {
    std::reference_wrapper<const EntryDef_> entryDef;
    dm::EntryValueType_t<EntryDef_> value;

    bool operator()(const dm::FullMarsRecord& rec) const {
        const auto& entry = entryDef.get().get(rec);
        return (entry.isSet() && (OpFunctor{}(entry.get(), value)));
    }
};
template <typename EntryDef_, typename OpFunctor>
MatchOp(const EntryDef_&, OpFunctor&&) -> MatchOp<EntryDef_, std::decay_t<OpFunctor>>;

template <typename EntryDef_>
using GreaterThan = MatchOp<EntryDef_, std::greater<>>;
template <typename EntryDef_>
auto greaterThan(const EntryDef_& e, dm::EntryValueType_t<EntryDef_> v) {
    return GreaterThan<EntryDef_>{e, v};
}
template <typename EntryDef_>
using GreaterEqual = MatchOp<EntryDef_, std::greater_equal<>>;
template <typename EntryDef_>
auto greaterEqual(const EntryDef_& e, dm::EntryValueType_t<EntryDef_> v) {
    return GreaterEqual<EntryDef_>{e, v};
}
template <typename EntryDef_>
using LessThan = MatchOp<EntryDef_, std::less<>>;
template <typename EntryDef_>
auto lessThan(const EntryDef_& e, dm::EntryValueType_t<EntryDef_> v) {
    return LessThan<EntryDef_>{e, v};
}
template <typename EntryDef_>
using LessEqual = MatchOp<EntryDef_, std::less_equal<>>;
template <typename EntryDef_>
auto lessEqual(const EntryDef_& e, dm::EntryValueType_t<EntryDef_> v) {
    return LessEqual<EntryDef_>{e, v};
}


//-----------------------------------------------------------------------------
// Composed matchers ...
// these should not be templated
//-----------------------------------------------------------------------------


// Compose a set of matchers with a fold AND expression
template <typename Matcher, typename... Matchers>
struct All {
    std::tuple<Matcher, Matchers...> matchers;

    bool operator()(const dm::FullMarsRecord& rec) const {
        return std::apply([&](const auto&... mx) { return (mx(rec) && ... && true); }, matchers);
    }
};

template <typename Matcher, typename... Matchers>
auto all(Matcher&& matcher, Matchers&&... matchers) {
    return All<std::decay_t<Matcher>, std::decay_t<Matchers>...>{
        std::make_tuple(std::forward<Matcher>(matcher), std::forward<Matchers>(matchers)...)};
}


// Compose a set of matchers with a fold OR expression
template <typename Matcher, typename... Matchers>
struct Any {
    std::tuple<Matcher, Matchers...> matchers;

    bool operator()(const dm::FullMarsRecord& rec) const {
        return std::apply([&](const auto&... mx) { return (mx(rec) || ... || false); }, matchers);
    }
};

template <typename Matcher, typename... Matchers>
auto any(Matcher&& matcher, Matchers&&... matchers) {
    return Any<std::decay_t<Matcher>, std::decay_t<Matchers>...>{
        std::make_tuple(std::forward<Matcher>(matcher), std::forward<Matchers>(matchers)...)};
}


}  // namespace multio::mars2grib::matcher


namespace multio::util {
template <typename EntryDef_>
struct Print<mars2grib::matcher::Range<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::Range<EntryDef_>& r) {
        ps << "Range<" << r.entryDef.get().key() << ">(" << r.first << ", " << r.last << ")";
    }
};


template <typename EntryDef_>
struct Print<mars2grib::matcher::Ranges<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::Ranges<EntryDef_>& r) {
        ps << "Ranges(";
        bool first = true;
        for (const auto& ri : r.ranges) {
            if (first) {
                first = false;
                ps << "  ";
            }
            else {
                ps << ", ";
            }
            {
                IndentGuard g(ps);
                ps << ri;
            }
            ps.softBreak();
        }
        ps << ")";
    }
};

template <typename EntryDef_>
struct Print<mars2grib::matcher::OneOf<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::OneOf<EntryDef_>& r) {
        ps << "OneOf<" << r.entryDef.get().key() << ">(";
        bool first = true;
        for (const auto& ri : r.values) {
            if (first) {
                first = false;
            }
            else {
                ps << ", ";
            }
            ps << ri;
        }
        ps << ")";
    }
};


template <typename EntryDef_>
struct Print<mars2grib::matcher::NoneOf<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::NoneOf<EntryDef_>& r) {
        ps << "NoneOf<" << r.entryDef.get().key() << ">(";
        bool first = true;
        for (const auto& ri : r.values) {
            if (first) {
                first = false;
            }
            else {
                ps << ", ";
            }
            ps << ri;
        }
        ps << ")";
    }
};


template <typename EntryDef_>
struct Print<mars2grib::matcher::Has<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::Has<EntryDef_>& r) {
        ps << "Has<" << r.entryDef.get().key() << ">()";
    }
};

template <typename EntryDef_>
struct Print<mars2grib::matcher::Missing<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::Missing<EntryDef_>& r) {
        ps << "Missing<" << r.entryDef.get().key() << ">()";
    }
};


template <typename EntryDef_>
struct Print<mars2grib::matcher::GreaterThan<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::GreaterThan<EntryDef_>& m) {
        ps << "GreaterThan<" << m.entryDef.get().key() << ">(" << m.value << ")";
    }
};

template <typename EntryDef_>
struct Print<mars2grib::matcher::GreaterEqual<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::GreaterEqual<EntryDef_>& m) {
        ps << "GreaterEqual<" << m.entryDef.get().key() << ">(" << m.value << ")";
    }
};

template <typename EntryDef_>
struct Print<mars2grib::matcher::LessThan<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::LessThan<EntryDef_>& m) {
        ps << "LessThan<" << m.entryDef.get().key() << ">(" << m.value << ")";
    }
};

template <typename EntryDef_>
struct Print<mars2grib::matcher::LessEqual<EntryDef_>> {
    static void print(PrintStream& ps, const mars2grib::matcher::LessEqual<EntryDef_>& m) {
        ps << "LessEqual<" << m.entryDef.get().key() << ">(" << m.value << ")";
    }
};


template <typename... Matchers>
struct Print<mars2grib::matcher::All<Matchers...>> {
    template <typename M>
    static void printMatcher(bool& first, PrintStream& ps, const M& m) {
        if (first) {
            first = false;
            ps << "  ";
        }
        else {
            ps << ", ";
        }
        {
            IndentGuard g(ps);
            ps << m;
            ps.softBreak();
        }
    }
    static void print(PrintStream& ps, const mars2grib::matcher::All<Matchers...>& a) {
        ps << "all(";
        ps.softBreak();
        bool first = true;
        std::apply([&](const auto&... matchers) { (printMatcher(first, ps, matchers), ...); }, a.matchers);
        ps << ")";
    }
};


template <typename... Matchers>
struct Print<mars2grib::matcher::Any<Matchers...>> {
    template <typename M>
    static void printMatcher(bool& first, PrintStream& ps, const M& m) {
        if (first) {
            first = false;
            ps << "  ";
        }
        else {
            ps << ", ";
        }
        {
            IndentGuard g(ps);
            ps << m;
            ps.softBreak();
        }
    }
    static void print(PrintStream& ps, const mars2grib::matcher::Any<Matchers...>& a) {
        ps << "any(";
        ps.softBreak();
        bool first = true;
        std::apply([&](const auto&... matchers) { (printMatcher(first, ps, matchers), ...); }, a.matchers);
        ps << ")";
    }
};

}  // namespace multio::util

