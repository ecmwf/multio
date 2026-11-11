/*
 * (C) Copyright 2022- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Simon Smart
/// @date Nov 2022

#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <unordered_set>
#include <variant>
#include <vector>

#include "eckit/types/Types.h"

#include "Metadata.h"

namespace eckit {
class LocalConfiguration;
}

namespace multio::message::match {

//--------------------------------------------------------------------------------------------------

/// Numeric value used by conditions. Integers are compared exactly, mixed or floating point values as doubles.
using Number = std::variant<std::int64_t, double>;

/// Extracts a number from a metadata value (int64, double or float). Returns nullopt for any other type.
std::optional<Number> toNumber(const MetadataValue& v);


/// Arithmetic progression (or plain interval if `by` is not given): from <= v [<= to] [and (v - from) % by == 0]
struct Range {
    Number from;
    std::optional<Number> to;
    std::optional<Number> by;

    bool matches(const Number& v) const;
};

enum class Comparison : unsigned
{
    GreaterThan,
    GreaterEqualTo,
    LowerThan,
    LowerEqualTo,
};

struct Bound {
    Comparison op;
    Number value;

    bool matches(const Number& v) const;
};

/// Condition on a single numeric key, configured with a map instead of a list of values:
///
///   step:
///     from: [0, 24, 144]          # scalars or lists; lists must have equal length, scalars are broadcast
///     to: [24, 144, 360]          # optional, inclusive
///     by: [1, 3, 6]               # optional
///     greater-than: 0             # optional bounds, all of them must hold
///     greater-equal-to: 0
///     lower-than: 360
///     lower-equal-to: 360
///
/// A value matches if it matches any of the from/to/by segments (if given) and all bounds.
class NumericCondition {
public:
    NumericCondition(const eckit::LocalConfiguration& cfg, const std::string& key);

    bool matches(const Number& v) const;

private:
    friend std::ostream& operator<<(std::ostream& os, const NumericCondition& c) {
        c.print(os);
        return os;
    }

    void print(std::ostream& os) const;

    std::vector<Range> ranges_;  // OR
    std::vector<Bound> bounds_;  // AND
};

/// What a single key is matched against: a set of exact values and/or numeric conditions (OR of all of them)
struct KeyMatcher {
    std::unordered_set<MetadataValue> values;
    std::vector<NumericCondition> conditions;
};

//--------------------------------------------------------------------------------------------------


enum class Predicate : unsigned
{
    None = 0,
    Negate = 1,
};

enum class Reduce : unsigned
{
    Or = 0,
    And = 1,
};


//--------------------------------------------------------------------------------------------------

class MatchKeys {

public:  // methods
    explicit MatchKeys(const eckit::LocalConfiguration& cfg, Predicate p, bool enforceMatchKeys);

    bool matches(const Metadata& md) const;

private:  // methods
    friend std::ostream& operator<<(std::ostream& os, const MatchKeys& m) {
        m.print(os);
        return os;
    }

    void print(std::ostream& os) const;

    void negate();
    void applyPredicate(Predicate);

private:  // members
    Predicate predicate_;
    bool enforceSameKeyTypes_;
    // Use vectorbecause we only iterate over key-pair values
    std::vector<std::pair<typename MetadataTypes::KeyType, KeyMatcher>> matcher_;
};

//--------------------------------------------------------------------------------------------------

class MatchReduce {
public:  // methods
    // SharedPoiter just used to defer construction - unique_ptr can be used as well but requires defining copy
    // construction & assignment
    using Elem = std::variant<MatchKeys, std::shared_ptr<MatchReduce>>;
    using Container = std::vector<Elem>;

    // MatchReduce(MatchReduce const&);
    // MatchReduce(MatchReduce&&) = default;

    // MatchReduce& operator=(MatchReduce const&);
    // MatchReduce& operator=(MatchReduce&&) noexcept = default;

    explicit MatchReduce(const eckit::LocalConfiguration& cfg, Predicate p = Predicate::None);
    explicit MatchReduce(Reduce r = Reduce::And, Predicate p = Predicate::None);

    bool matches(const Metadata& md) const;

    bool isEmpty() const;

    void extend(const MatchKeys&);
    void extend(MatchKeys&&);
    void extend(const MatchReduce&);
    void extend(MatchReduce&&);
    void extend(const Elem&);
    void extend(Elem&&);

    void negate();
    void applyPredicate(Predicate);

    static MatchReduce construct(const eckit::LocalConfiguration&, Predicate p = Predicate::None,
                                 bool enforceSameKeyTypesParent = true);


private:  // methods
    friend std::ostream& operator<<(std::ostream& os, const MatchReduce& m) {
        m.print(os);
        return os;
    }

    void print(std::ostream& os) const;

private:  // members
    Container matchers_;
    Reduce reduce_;
    Predicate predicate_;
};


//--------------------------------------------------------------------------------------------------

}  // namespace multio::message::match
