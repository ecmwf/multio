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

#include "multio/mars2grib/EncoderConf.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/rules/Setter.h"

#include "multio/datamod/MarsMiscGeo.h"

#include "multio/util/Print.h"

#include <memory>
#include <sstream>
#include <vector>


namespace multio::mars2grib::rules {

namespace dm = multio::datamod;

// Rule with dynamic dispatch to be stored in containers massively
struct DynRule {
    // Combines matching and setting. If matched on `keys`, the setter is applied and true is returned.
    // If nothing matches only false is returned
    virtual bool apply(const dm::MarsRecord& rec, SectionsConf&) const = 0;
    virtual void print(util::PrintStream&) const = 0;

    virtual ~DynRule() = default;
};
}  // namespace multio::mars2grib::rules

namespace multio::util {

template <>
struct Print<mars2grib::rules::DynRule> {
    static void print(util::PrintStream& ps, const mars2grib::rules::DynRule& r) { r.print(ps); }
};
}  // namespace multio::util

namespace multio::mars2grib::rules {

template <typename Derived>
struct DerivedRule : DynRule {
    bool apply(const dm::MarsRecord& rec, SectionsConf& conf) const override {
        return static_cast<const Derived&>(*this)(rec, conf);
    }

    void print(util::PrintStream& ps) const override { util::print(ps, static_cast<const Derived&>(*this)); }
};

}  // namespace multio::mars2grib::rules

namespace multio::util {}

namespace multio::mars2grib::rules {


// A Rule combining matcher and a setter
// Hence its operator() takes a keyset for matching, a encoder configuration to set keys, and it returns the result of
// the match. Nothing is set if the match fails.
template <typename Matcher>
struct Rule : DerivedRule<Rule<Matcher>> {
    Rule(Matcher&& m, Setter&& s) : matcher{std::move(m)}, setter{std::move(s)} {}
    Matcher matcher;
    Setter setter;

    // Match and set
    bool operator()(const dm::MarsRecord& rec, SectionsConf& conf) const {
        if (matcher(rec)) {
            setter(conf);
            return true;
        }
        return false;
    }
};

// Rule maker
template <typename Matcher_>
auto rule(Matcher_&& matcher, Setter&& setter) {
    return Rule<std::decay_t<Matcher_>>{std::forward<Matcher_>(matcher), std::move(setter)};
}

// Rule maker with a NoOp (to just match)
template <typename Matcher_>
auto rule(Matcher_&& matcher) {
    return Rule<std::decay_t<Matcher_>>{std::forward<Matcher_>(matcher), Setter([](SectionsConf&) {})};
}

// Rule with a matcher and mustiple setters (which get combined with `setAll`)
template <typename Matcher_, typename... Setters_, std::enable_if_t<(sizeof...(Setters_) >= 2), bool> = true>
auto rule(Matcher_&& matcher, Setters_&&... setters) {
    return rule(std::forward<Matcher_>(matcher), setAll(std::forward<Setters_>(setters)...));
}

}  // namespace multio::mars2grib::rules

namespace multio::util {
template <typename Matcher>
struct Print<mars2grib::rules::Rule<Matcher>> {
    static void print(util::PrintStream& ps, const mars2grib::rules::Rule<Matcher>& r) {
        ps << "rule(" << std::endl;
        ps << "  ";
        {
            IndentGuard g(ps);
            ps << r.matcher << std::endl;
        }
        ps << ")";
    }
};

}  // namespace multio::util

namespace multio::mars2grib::rules {


// An ExclusiveRuleList contains a list of rules from which only one is expected to match and be applied
// This concept is important to express combinations of orthogonal rules
struct ExclusiveRuleList : DerivedRule<ExclusiveRuleList> {
    std::vector<std::unique_ptr<DynRule>> rules;

    // Match and set
    bool operator()(const dm::MarsRecord& rec, SectionsConf& conf) const;
};


template <typename Rule_, typename... Rules_>
ExclusiveRuleList exclusiveRuleList(Rule_&& rule, Rules_&&... rules) {
    ExclusiveRuleList res;
    res.rules.emplace_back(std::make_unique<std::decay_t<Rule_>>(std::forward<Rule_>(rule)));
    (res.rules.emplace_back(std::make_unique<std::decay_t<Rules_>>(std::forward<Rules_>(rules))), ...);
    return res;
}


ExclusiveRuleList mergeRuleList(ExclusiveRuleList&& res);

template <typename... More>
ExclusiveRuleList mergeRuleList(ExclusiveRuleList&& res, ExclusiveRuleList&& next, More&&... more) {
    res.rules.insert(res.rules.end(), std::make_move_iterator(next.rules.begin()),
                     std::make_move_iterator(next.rules.end()));

    return mergeRuleList(std::move(res), std::forward<More>(more)...);
}

}  // namespace multio::mars2grib::rules

namespace multio::util {
template <>
struct Print<mars2grib::rules::ExclusiveRuleList> {
    static void print(util::PrintStream& ps, const mars2grib::rules::ExclusiveRuleList& r);
};
}  // namespace multio::util

namespace multio::mars2grib::rules {


// Chains multiple rules on a struct matter.
// If the first rule applies, all others also have to apply - otherwise an exception is thrown to indicate that the
// key set is not definitely mapped. If the first rule does not apply, false is returned (and indicates that no
// modification happened) This is expected to be compbined with multiple ExclusiveRuleList to eventually form a
// combination of all partial rules.
struct ChainedRuleList : DerivedRule<ChainedRuleList> {
    std::vector<std::unique_ptr<DynRule>> rules;

    // Match and set
    bool operator()(const dm::MarsRecord& rec, SectionsConf& conf) const;
};


template <typename Rule_, typename... Rules_>
ChainedRuleList chainedRuleList(Rule_&& rule, Rules_&&... rules) {
    ChainedRuleList res;
    res.rules.emplace_back(std::make_unique<std::decay_t<Rule_>>(std::forward<Rule_>(rule)));
    (res.rules.emplace_back(std::make_unique<std::decay_t<Rules_>>(std::forward<Rules_>(rules))), ...);
    return res;
}

}  // namespace multio::mars2grib::rules


namespace multio::util {


template <>
struct Print<mars2grib::rules::ChainedRuleList> {
    static void print(util::PrintStream& ps, const mars2grib::rules::ChainedRuleList& r);
};


}  // namespace multio::util

