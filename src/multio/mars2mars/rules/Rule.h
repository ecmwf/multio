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

#include "multio/mars2mars/rules/Matcher.h"
#include "multio/mars2mars/MappingResult.h"
#include "multio/mars2mars/Mars2MarsException.h"
#include "multio/mars2mars/rules/Setter.h"

#include "multio/util/Print.h"

#include <initializer_list>
#include <memory>
#include <sstream>
#include <vector>


namespace multio::mars2mars::rules {

namespace dm = multio::datamod;

// Rule with dynamic dispatch to be stored in containers massively
struct DynRule {
    // Combines matching and setting. If matched on `keys`, the setter is applied and true is returned.
    // If nothing matches only false is returned
    virtual std::optional<MappingResult> apply(dm::FullMarsRecord&, dm::MiscRecord&) const = 0;
    virtual void print(util::PrintStream&) const = 0;
};


template <typename Derived>
struct DerivedRule : DynRule {
    std::optional<MappingResult> apply(dm::FullMarsRecord& marsVals, dm::MiscRecord& miscVals) const override {
        return static_cast<const Derived&>(*this)(marsVals, miscVals);
    }

    void print(util::PrintStream& ps) const override { util::print(ps, static_cast<const Derived&>(*this)); }
};


// A Rule combining matcher and a setter
// Hence its operator() takes a keyset for matching, a encoder configuration to set keys, and it returns the result of
// the match. Nothing is set if the match fails.
template <typename Matcher, typename Setter>
struct Rule : DerivedRule<Rule<Matcher, Setter>> {
    Rule(Matcher m, Setter s) : matcher{std::move(m)}, setter{std::move(s)} {}
    Matcher matcher;
    Setter setter;

    // Match and set
    std::optional<MappingResult> operator()(dm::FullMarsRecord& marsVals, dm::MiscRecord& miscVals) const {
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
    return Rule<std::decay_t<Matcher_>, std::decay_t<Setter_>>{std::forward<Matcher_>(matcher),
                                                               std::forward<Setter_>(setter)};
}


// Rule with a matcher and mustiple setters (which get combined with `setAll`)
template <typename Matcher_, typename... Setters_, std::enable_if_t<(sizeof...(Setters_) >= 2), bool> = true>
auto rule(Matcher_&& matcher, Setters_&&... setters) {
    return rule(std::forward<Matcher_>(matcher), setAll(std::forward<Setters_>(setters)...));
}


struct RuleList : DerivedRule<RuleList> {
    using RuleEntry = std::variant<std::unique_ptr<DynRule>, std::reference_wrapper<const RuleList>>;
    std::vector<RuleEntry> rules;

    // Match and set
    std::optional<MappingResult> operator()(dm::FullMarsRecord& marsVals, dm::MiscRecord& miscVals) const;


    // Construction helper
    void addEntry(const RuleList& ruleList) { rules.emplace_back(std::cref(ruleList)); }
    void addEntry(RuleList&& ruleList) {
        for (auto&& rule : std::move(ruleList.rules)) {
            rules.emplace_back(std::move(rule));
        }
    }
    template <typename Rule_, std::enable_if_t<!std::is_same_v<std::decay_t<Rule_>, RuleList>, bool> = true>
    void addEntry(Rule_&& rule) {
        rules.emplace_back(std::make_unique<std::decay_t<Rule_>>(std::forward<Rule_>(rule)));
    }
};


template <typename Rule_, typename... Rules_>
RuleList ruleList(Rule_&& rule, Rules_&&... rules) {
    RuleList res;
    res.addEntry(std::forward<Rule_>(rule));
    (void)std::initializer_list<int>{(res.addEntry(std::forward<Rules_>(rules)), 0)...};
    return res;
}


}  // namespace multio::mars2mars::rules


namespace multio::util {

template <typename Matcher, typename Setter>
struct Print<mars2mars::rules::Rule<Matcher, Setter>> {
    static void print(PrintStream& ps, const mars2mars::rules::Rule<Matcher, Setter>& r) {
        ps << "rule(";
        ps.softBreak();
        {
            IndentGuard g(ps);
            ps << "  " << r.matcher;
            ps.softBreak();
            ps << ", " << r.setter;
            ps.softBreak();
        }
        ps << ")";
    }
};


template <>
struct Print<mars2mars::rules::DynRule> {
    static void print(PrintStream& os, const mars2mars::rules::DynRule& r);
};


template <>
struct Print<mars2mars::rules::RuleList> {
    static void print(PrintStream& os, const mars2mars::rules::RuleList& r);
};


}  // namespace multio::util

