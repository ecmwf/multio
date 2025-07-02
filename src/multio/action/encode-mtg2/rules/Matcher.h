/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/util/TypeToString.h"

#include <functional>
#include <iostream>
#include <tuple>
#include <vector>


namespace multio::action::rules {

// Range of values to match
template <auto Id_>
struct Range {
    using KeySet = datamod::KeySet<decltype(Id_)>;
    using ValueType = datamod::KeyDefValueType_t<Id_>;

    ValueType first;
    ValueType last;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        const auto& kv = datamod::key<Id_>(keys);

        return (kv.has() && ((kv.get() >= first) && (kv.get() <= last)));
    }
};

template <auto id_>
std::ostream& operator<<(std::ostream& os, const Range<id_>& r) {
    os << "Range<" << util::typeToString<datamod::KeyId<id_>>() << ">(" << r.first << ", " << r.last << ")";
    return os;
}

// List of ranges
template <auto Id_>
struct Ranges {
    using KeySet = datamod::KeySet<decltype(Id_)>;
    using ValueType = datamod::KeyDefValueType_t<Id_>;

    std::vector<Range<Id_>> ranges;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        const auto& kv = datamod::key<Id_>(keys);

        for (const auto& range : ranges) {
            if (range(keys)) {
                return true;
            }
        }
        return false;
    }
};


template <auto id_>
std::ostream& operator<<(std::ostream& os, const Ranges<id_>& r) {
    os << "Ranges<" << util::typeToString<datamod::KeyId<id_>>() << ">(";
    bool first = true;
    for (const auto& ri : r.ranges) {
        if (first) {
            first = false;
        }
        else {
            os << ", ";
        }
        os << ri;
    }
    os << ")";
    return os;
}


// Check if the value of a field matches any of the set of values given
// Returns false if the field is not given
template <auto Id_>
struct OneOf {
    using KeySet = datamod::KeySet<decltype(Id_)>;
    using ValueType = datamod::KeyDefValueType_t<Id_>;

    // Using a vector because the only operation we perform is search, mostly on a few elements.
    // Having contiguous memory access is helpful here
    std::vector<ValueType> values;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        const auto& kv = datamod::key<Id_>(keys);

        return (kv.has() && (std::find(values.begin(), values.end(), kv.get()) != values.end()));
    }
};


template <auto id_>
std::ostream& operator<<(std::ostream& os, const OneOf<id_>& r) {
    os << "OneOf<" << util::typeToString<datamod::KeyId<id_>>() << ">(";
    bool first = true;
    for (const auto& ri : r.values) {
        if (first) {
            first = false;
        }
        else {
            os << ", ";
        }
        os << ri;
    }
    os << ")";
    return os;
}

// Check if the value of a field is not in a given exclusion list
// Returns false if the field is not given
template <auto Id_>
struct NoneOf {
    using KeySet = datamod::KeySet<decltype(Id_)>;
    using ValueType = datamod::KeyDefValueType_t<Id_>;

    std::vector<ValueType> values;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        const auto& kv = datamod::key<Id_>(keys);

        return (kv.has() && (std::find(values.begin(), values.end(), kv.get()) == values.end()));
    }
};

template <auto id_>
std::ostream& operator<<(std::ostream& os, const NoneOf<id_>& r) {
    os << "NoneOf<" << util::typeToString<datamod::KeyId<id_>>() << ">(";
    bool first = true;
    for (const auto& ri : r.values) {
        if (first) {
            first = false;
        }
        else {
            os << ", ";
        }
        os << ri;
    }
    os << ")";
    return os;
}

// Checks if a field is given
template <auto Id_>
struct Has {
    using KeySet = datamod::KeySet<decltype(Id_)>;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        const auto& kv = datamod::key<Id_>(keys);
        return kv.has();
    }
};

template <auto id_>
std::ostream& operator<<(std::ostream& os, const Has<id_>& r) {
    os << "Has<" << util::typeToString<datamod::KeyId<id_>>() << ">()";
    return os;
}


// Checks if a field is missing
template <auto Id_>
struct Missing {
    using KeySet = datamod::KeySet<decltype(Id_)>;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        const auto& kv = datamod::key<Id_>(keys);
        return kv.isMissing();
    }
};

template <auto id_>
std::ostream& operator<<(std::ostream& os, const Missing<id_>& r) {
    os << "Missing<" << util::typeToString<datamod::KeyId<id_>>() << ">()";
    return os;
}


// Match a binary operation like >, >=, <, <=
template <auto Id_, typename OpFunctor>
struct MatchOp {
    using KeySet = datamod::KeySet<decltype(Id_)>;
    using ValueType = datamod::KeyDefValueType_t<Id_>;

    ValueType value;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        const auto& kv = datamod::key<Id_>(keys);

        return (kv.has() && (OpFunctor{}(kv.get(), value)));
    }
};

template <auto Id_>
using GreaterThan = MatchOp<Id_, std::greater<>>;
template <auto Id_>
using GreaterEqual = MatchOp<Id_, std::greater_equal<>>;
template <auto Id_>
using LessThan = MatchOp<Id_, std::less<>>;
template <auto Id_>
using LessEqual = MatchOp<Id_, std::less_equal<>>;

template <auto id_>
std::ostream& operator<<(std::ostream& os, const GreaterThan<id_>& m) {
    os << "GreaterThan<" << util::typeToString<datamod::KeyId<id_>>() << ">(" << m.value << ")";
    return os;
}
template <auto id_>
std::ostream& operator<<(std::ostream& os, const GreaterEqual<id_>& m) {
    os << "GreaterEqual<" << util::typeToString<datamod::KeyId<id_>>() << ">(" << m.value << ")";
    return os;
}
template <auto id_>
std::ostream& operator<<(std::ostream& os, const LessThan<id_>& m) {
    os << "LessThan<" << util::typeToString<datamod::KeyId<id_>>() << ">(" << m.value << ")";
    return os;
}
template <auto id_>
std::ostream& operator<<(std::ostream& os, const LessEqual<id_>& m) {
    os << "LessEqual<" << util::typeToString<datamod::KeyId<id_>>() << ">(" << m.value << ")";
    return os;
}


// Compose a set of matchers with a fold AND expression
template <typename Matcher, typename... Matchers>
struct All {
    using KeySet = typename Matcher::KeySet;
    std::tuple<Matcher, Matchers...> matchers;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        return std::apply([&](const auto&... mx) { return (mx(keys) && ... && true); }, matchers);
    }
};

template <typename Matcher, typename... Matchers>
auto all(Matcher&& matcher, Matchers&&... matchers) {
    return All<std::decay_t<Matcher>, std::decay_t<Matchers>...>{
        std::make_tuple(std::forward<Matcher>(matcher), std::forward<Matchers>(matchers)...)};
}


template <typename... Matchers>
std::ostream& operator<<(std::ostream& os, const All<Matchers...>& a) {
    os << "all(";
    std::apply([&](const auto&... matchers) { ((os << matchers << ", "), ...); }, a.matchers);
    os << ")";
    return os;
}


// Compose a set of matchers with a fold OR expression
template <typename Matcher, typename... Matchers>
struct Any {
    using KeySet = typename Matcher::KeySet;
    std::tuple<Matcher, Matchers...> matchers;

    bool operator()(const datamod::KeyValueSet<KeySet>& keys) const {
        return std::apply([&](const auto&... mx) { return (mx(keys) || ... || false); }, matchers);
    }
};

template <typename Matcher, typename... Matchers>
auto any(Matcher&& matcher, Matchers&&... matchers) {
    return Any<std::decay_t<Matcher>, std::decay_t<Matchers>...>{
        std::make_tuple(std::forward<Matcher>(matcher), std::forward<Matchers>(matchers)...)};
}

template <typename... Matchers>
std::ostream& operator<<(std::ostream& os, const Any<Matchers...>& a) {
    os << "any(";
    std::apply([&](const auto&... matchers) { ((os << matchers << ", "), ...); }, a.matchers);
    os << ")";
    return os;
}


}  // namespace multio::action::rules

