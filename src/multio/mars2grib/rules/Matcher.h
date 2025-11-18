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

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/util/Print.h"

#include <functional>
#include <tuple>
#include <vector>


namespace multio::mars2grib::matcher {

namespace dm = multio::datamod;

// Matchers are defined on a key that serves as accessor and transports type information ... this saves defining a lot
// functions.
// Alternatively they could be defined for a given value type.

// Range of values to match
template <typename MarsRec, typename ValueType>
struct Range {
    ValueType MarsRec::* member;
    dm::EntryValueType_t<ValueType> first;
    dm::EntryValueType_t<ValueType> last;

    bool operator()(const dm::FullMarsRecord& rec) const {
        const auto& entry = rec.*member;

        return (entry.isSet() && ((entry.get() >= first) && (entry.get() <= last)));
    }
};

template <typename MarsRec, typename ValueType, typename... Args>
Range(ValueType MarsRec::*, ValueType, ValueType) -> Range<MarsRec, ValueType>;


// List of ranges
template <typename MarsRec, typename ValueType>
struct Ranges {
    std::vector<Range<MarsRec, ValueType>> ranges;

    bool operator()(const dm::FullMarsRecord& rec) const {
        for (const auto& range : ranges) {
            if (range(rec)) {
                return true;
            }
        }
        return false;
    }
};
template <typename MarsRec, typename ValueType, typename... Args>
Ranges(ValueType MarsRec::*, Args&&...) -> Ranges<MarsRec, ValueType>;


// Check if the value of a field matches any of the set of values given
// Returns false if the field is not given
template <typename MarsRec, typename ValueType>
struct OneOf {
    // Using a vector because the only operation we perform is search, mostly on a few elements.
    // Having contiguous memory access is helpful here
    ValueType MarsRec::* member;
    std::vector<dm::EntryValueType_t<ValueType>> values;

    bool operator()(const MarsRec& rec) const {
        const auto& entry = rec.*member;

        return (entry.isSet() && (std::find(values.begin(), values.end(), entry.get()) != values.end()));
    }
};

template <typename MarsRec, typename ValueType>
OneOf(ValueType MarsRec::*, std::vector<dm::EntryValueType_t<ValueType>>) -> OneOf<MarsRec, ValueType>;


// Check if the value of a field is not in a given exclusion list
// Returns false if the field is not given
template <typename MarsRec, typename ValueType>
struct NoneOf {
    ValueType MarsRec::* member;
    std::vector<dm::EntryValueType_t<ValueType>> values;

    bool operator()(const dm::FullMarsRecord& rec) const {
        const auto& entry = rec.*member;

        return !entry.isSet() || (std::find(values.begin(), values.end(), entry.get()) == values.end());
    }
};
template <typename MarsRec, typename ValueType>
NoneOf(ValueType MarsRec::*, std::vector<dm::EntryValueType_t<ValueType>>) -> NoneOf<MarsRec, ValueType>;


// Checks if a field is given
template <typename MarsRec, typename ValueType>
struct Has {
    ValueType MarsRec::* member;

    bool operator()(const dm::FullMarsRecord& rec) const { return (rec.*member).isSet(); }
};
template <typename MarsRec, typename ValueType>
Has(ValueType MarsRec::*) -> Has<MarsRec, ValueType>;


// Checks if a field is missing
template <typename MarsRec, typename ValueType>
struct Missing {
    ValueType MarsRec::* member;

    bool operator()(const dm::FullMarsRecord& rec) const { return !(rec.*member).isSet(); }
};
template <typename MarsRec, typename ValueType>
Missing(ValueType MarsRec::*) -> Missing<MarsRec, ValueType>;


// Match a binary operation like >, >=, <, <=
template <typename MarsRec, typename ValueType, typename OpFunctor>
struct MatchOp {
    ValueType MarsRec::* member;
    dm::EntryValueType_t<ValueType> value;

    bool operator()(const dm::FullMarsRecord& rec) const {
        const auto& entry = rec.*member;
        return (entry.isSet() && (OpFunctor{}(entry.get(), value)));
    }
};
template <typename MarsRec, typename ValueType, typename OpFunctor>
MatchOp(ValueType MarsRec::*, OpFunctor&&) -> MatchOp<MarsRec, ValueType, std::decay_t<OpFunctor>>;

template <typename MarsRec, typename ValueType>
using GreaterThan = MatchOp<MarsRec, ValueType, std::greater<>>;
template <typename MarsRec, typename ValueType>
auto greaterThan(ValueType MarsRec::* e, dm::EntryValueType_t<ValueType> v) {
    return GreaterThan<MarsRec, ValueType>{e, v};
}
template <typename MarsRec, typename ValueType>
using GreaterEqual = MatchOp<MarsRec, ValueType, std::greater_equal<>>;
template <typename MarsRec, typename ValueType>
auto greaterEqual(ValueType MarsRec::* e, dm::EntryValueType_t<ValueType> v) {
    return GreaterEqual<MarsRec, ValueType>{e, v};
}
template <typename MarsRec, typename ValueType>
using LessThan = MatchOp<MarsRec, ValueType, std::less<>>;
template <typename MarsRec, typename ValueType>
auto lessThan(ValueType MarsRec::* e, dm::EntryValueType_t<ValueType> v) {
    return LessThan<MarsRec, ValueType>{e, v};
}
template <typename MarsRec, typename ValueType>
using LessEqual = MatchOp<MarsRec, ValueType, std::less_equal<>>;
template <typename MarsRec, typename ValueType>
auto lessEqual(ValueType MarsRec::* e, dm::EntryValueType_t<ValueType> v) {
    return LessEqual<MarsRec, ValueType>{e, v};
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
template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::Range<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::Range<MarsRec, ValueType>& r) {
        ps << "Range(" << r.first << ", " << r.last << ")";
    }
};


template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::Ranges<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::Ranges<MarsRec, ValueType>& r) {
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

template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::OneOf<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::OneOf<MarsRec, ValueType>& r) {
        ps << "OneOf(";
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


template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::NoneOf<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::NoneOf<MarsRec, ValueType>& r) {
        ps << "NoneOf(";
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


template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::Has<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::Has<MarsRec, ValueType>& r) {
        // TODO(pgeier) pretty printing needs to be fixed once the records use PointerToMember accessor only
        // then the proper readable key can be retrieved by checkend which entry in `record_entries_.member == r.member`
        ps << "Has()";
    }
};

template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::Missing<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::Missing<MarsRec, ValueType>& r) { ps << "Missing()"; }
};


template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::GreaterThan<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::GreaterThan<MarsRec, ValueType>& m) {
        ps << "GreaterThan(" << m.value << ")";
    }
};

template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::GreaterEqual<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::GreaterEqual<MarsRec, ValueType>& m) {
        ps << "GreaterEqual(" << m.value << ")";
    }
};

template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::LessThan<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::LessThan<MarsRec, ValueType>& m) {
        ps << "LessThan(" << m.value << ")";
    }
};

template <typename MarsRec, typename ValueType>
struct Print<mars2grib::matcher::LessEqual<MarsRec, ValueType>> {
    static void print(PrintStream& ps, const mars2grib::matcher::LessEqual<MarsRec, ValueType>& m) {
        ps << "LessEqual(" << m.value << ")";
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

