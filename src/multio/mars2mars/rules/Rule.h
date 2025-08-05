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

#include "multio/mars2grib/rules/Matcher.h"
#include "multio/mars2mars/MappingResult.h"
#include "multio/mars2mars/Mars2MarsException.h"
#include "multio/mars2mars/rules/Setter.h"

#include "multio/util/Print.h"

#include <memory>
#include <sstream>
#include <vector>


namespace multio::mars2mars::rules {

// Rule with dynamic dispatch to be stored in containers massively
struct DynRule {
    // Combines matching and setting. If matched on `keys`, the setter is applied and true is returned.
    // If nothing matches only false is returned
    virtual std::optional<MappingResult> apply(datamod::MarsKeyValueSet&, datamod::MiscKeyValueSet&) const = 0;
    virtual void print(std::ostream&) const = 0;

    virtual ~DynRule() = default;
};


template <typename Derived>
struct DerivedRule : DynRule {
    std::optional<MappingResult> apply(datamod::MarsKeyValueSet& marsVals,
                                       datamod::MiscKeyValueSet& miscVals) const override {
        return static_cast<const Derived&>(*this)(marsVals, miscVals);
    }

    void print(std::ostream& os) const override { util::print(os, static_cast<const Derived&>(*this)); }
};


// A Rule combining matcher and a setter
// Hence its operator() takes a keyset for matching, a encoder configuration to set keys, and it returns the result of
// the match. Nothing is set if the match fails.
template <typename Matcher, typename Setter>
struct Rule : DerivedRule<Rule<Matcher, Setter>> {
    Matcher matcher;
    Setter setter;

    // Match and set
    std::optional<MappingResult> operator()(datamod::MarsKeyValueSet& marsVals,
                                            datamod::MiscKeyValueSet& miscVals) const {
        if (matcher(marsVals)) {
            MappingResult res;
            setter.set(marsVals, miscVals, res);
            return res;
        }
        return {};
    }
};

// Rule maker
template <typename Matcher_, typename Setter_>
auto rule(Matcher_&& matcher, Setter_&& setter) {
    Rule<std::decay_t<Matcher_>, std::decay_t<Setter_>> res;
    res.matcher = std::forward<Matcher_>(matcher);
    res.setter = std::forward<Setter_>(setter);
    return res;
}


// Rule with a matcher and mustiple setters (which get combined with `setAll`)
template <typename Matcher_, typename... Setters_, std::enable_if_t<(sizeof...(Setters_) >= 2), bool> = true>
auto rule(Matcher_&& matcher, Setters_&&... setters) {
    return rule(std::forward<Matcher_>(matcher), setAll(std::forward<Setters_>(setters)...));
}


struct RuleList : DerivedRule<RuleList> {
    std::vector<std::unique_ptr<DynRule>> rules;

    // Match and set
    std::optional<MappingResult> operator()(datamod::MarsKeyValueSet& marsVals,
                                            datamod::MiscKeyValueSet& miscVals) const;
};


template <typename Rule_, typename... Rules_>
RuleList ruleList(Rule_&& rule, Rules_&&... rules) {
    RuleList res;
    res.rules.emplace_back(std::make_unique<std::decay_t<Rule_>>(std::forward<Rule_>(rule)));
    (res.rules.emplace_back(std::make_unique<std::decay_t<Rules_>>(std::forward<Rules_>(rules))), ...);
    return res;
}

}  // namespace multio::mars2mars::rules


namespace multio::util {

template <typename Matcher, typename Setter>
struct Print<mars2mars::rules::Rule<Matcher, Setter>> {
    static void print(std::ostream& os, const mars2mars::rules::Rule<Matcher, Setter>& r) {
        os << "rule(" << r.matcher << ", ";
        r.setter.print(os);
        os << ")";
    }
};


template <>
struct Print<mars2mars::rules::DynRule> {
    static void print(std::ostream& os, const mars2mars::rules::DynRule& r);
};


template <>
struct Print<mars2mars::rules::RuleList> {
    static void print(std::ostream& os, const mars2mars::rules::RuleList& r);
};


}  // namespace multio::util

