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

#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/action/encode-mtg2/rules/Matcher.h"
#include "multio/action/encode-mtg2/rules/Setter.h"

#include <memory>
#include <sstream>
#include <vector>


namespace multio::action::rules {

// Rule with dynamic dispatch to be stored in containers massively
template <typename MatchKeySet_>
struct DynRule {
    using MatchKeySet = MatchKeySet_;

    // Combines matching and setting. If matched on `keys`, the setter is applied and true is returned.
    // If nothing matches only false is returned
    virtual bool apply(const datamod::KeyValueSet<MatchKeySet_>& keys, EncoderSections&) const = 0;
    virtual void print(std::ostream&) const = 0;
    
    virtual ~DynRule() = default;
};


template <typename MatchKeySet>
std::ostream& operator<<(std::ostream& os, const DynRule<MatchKeySet>& r) {
    r.print(os);
    return os;
}


template <typename MatchKeySet_, typename Derived>
struct DerivedRule : DynRule<MatchKeySet_> {
    using MatchKeySet = MatchKeySet_;
    bool apply(const datamod::KeyValueSet<MatchKeySet_>& keys, EncoderSections& conf) const override {
        return static_cast<const Derived&>(*this)(keys, conf);
    }

    void print(std::ostream& os) const override {
        os << static_cast<const Derived&>(*this);
    }
};


// A Rule combining matcher and a setter
// Hence its operator() takes a keyset for matching, a encoder configuration to set keys, and it returns the result of
// the match. Nothing is set if the match fails.
template <typename Matcher, typename Setter>
struct Rule : DerivedRule<typename Matcher::KeySet, Rule<Matcher, Setter>> {
    using MatchKeySet = typename Matcher::KeySet;

    Matcher matcher;
    Setter setter;

    // Match and set
    bool operator()(const datamod::KeyValueSet<MatchKeySet>& kvs, EncoderSections& conf) const {
        if (matcher(kvs)) {
            setter.set(conf);
            return true;
        }
        return false;
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

// Rule maker with a NoOp (to just match)
template <typename Matcher_>
auto rule(Matcher_&& matcher) {
    Rule<std::decay_t<Matcher_>, NoOp> res;
    res.matcher = std::forward<Matcher_>(matcher);
    res.setter = NoOp{};
    return res;
    // return Rule<std::decay_t<Matcher_>, NoOp>(std::forward<Matcher_>(matcher), NoOp{});
}

// Rule with a matcher and mustiple setters (which get combined with `setAll`)
template <typename Matcher_, typename... Setters_, std::enable_if_t<(sizeof...(Setters_) >= 2), bool> = true>
auto rule(Matcher_&& matcher, Setters_&&... setters) {
    return rule(std::forward<Matcher_>(matcher), setAll(std::forward<Setters_>(setters)...));
}


template <typename Matcher, typename Setter>
std::ostream& operator<<(std::ostream& os, const Rule<Matcher, Setter>& r) {
    os << "rule(" << r.matcher << ", ";
    r.setter.print(os);
    os << ")";
    return os;
}


// An ExclusiveRuleList contains a list of rules from which only one is expected to match and be applied
// This concept is important to express combinations of orthogonal rules
template <typename MatchKeySet_>
struct ExclusiveRuleList : DerivedRule<MatchKeySet_, ExclusiveRuleList<MatchKeySet_>> {
    using MatchKeySet = MatchKeySet_;
    std::vector<std::unique_ptr<DynRule<MatchKeySet>>> rules;

    // Match and set
    bool operator()(const datamod::KeyValueSet<MatchKeySet>& kvs, EncoderSections& conf) const {
        DynRule<MatchKeySet>* appliedRule = nullptr;
        for (const auto& rule : rules) {
            if (rule->apply(kvs, conf)) {
                if (appliedRule != nullptr) {
                    std::ostringstream oss;
                    oss << "ExclusizeRuleList: Multiple rules apply although they should be exclusive.";
                    oss << " first match: " << *appliedRule << std::endl;
                    oss << " second match: " << *rule.get() << std::endl;
                    oss << " Keys: " << kvs;
                    throw EncodeMtg2Exception(oss.str(), Here());
                }
                appliedRule = rule.get();
            }
        }
        return (appliedRule != nullptr);
    }
};


template <typename Rule_, typename... Rules_>
ExclusiveRuleList<typename Rule_::MatchKeySet> exclusiveRuleList(Rule_&& rule, Rules_&&... rules) {
    using MatchKS = typename Rule_::MatchKeySet;
    ExclusiveRuleList<MatchKS> res;
    res.rules.emplace_back(std::make_unique<std::decay_t<Rule_>>(std::forward<Rule_>(rule)));
    (res.rules.emplace_back(std::make_unique<std::decay_t<Rules_>>(std::forward<Rules_>(rules))), ...);
    return res;
}


template <typename MatchKeySet_>
ExclusiveRuleList<MatchKeySet_> mergeRuleList(ExclusiveRuleList<MatchKeySet_>&& res) {
    return std::move(res);
}

template <typename MatchKeySet_, typename... More>
ExclusiveRuleList<MatchKeySet_> mergeRuleList(ExclusiveRuleList<MatchKeySet_>&& res,
                                              ExclusiveRuleList<MatchKeySet_>&& next, More&&... more) {
    res.rules.insert(res.rules.end(), std::make_move_iterator(next.rules.begin()),
                     std::make_move_iterator(next.rules.end()));

    return mergeRuleList(std::move(res), std::forward<More>(more)...);
}


template <typename MatchKeySet>
std::ostream& operator<<(std::ostream& os, const ExclusiveRuleList<MatchKeySet>& r) {
    os << "exclusiveRuleList(";
    bool first = true;
    for (const auto& ri : r.rules) {
        if (first) {
            first = false;
        }
        else {
            os << ", ";
        }
        os << *ri.get();
    }
    return os;
}


// Chains multiple rules on a struct matter.
// If the first rule applies, all others also have to apply - otherwise an exception is thrown to indicate that the
// key set is not definitely mapped. If the first rule does not apply, false is returned (and indicates that no
// modification happened) This is expected to be compbined with multiple ExclusiveRuleList to eventually form a
// combination of all partial rules.
template <typename MatchKeySet_>
struct ChainedRuleList : DerivedRule<MatchKeySet_, ChainedRuleList<MatchKeySet_>> {
    using MatchKeySet = MatchKeySet_;
    std::vector<std::unique_ptr<DynRule<MatchKeySet>>> rules;

    // Match and set
    bool operator()(const datamod::KeyValueSet<MatchKeySet>& kvs, EncoderSections& conf) const {
        bool first = true;
        for (const auto& rule : rules) {
            bool matched = rule->apply(kvs, conf);
            if (first) {
                // First failed - nothing has been applied yet
                if (!matched) {
                    return false;
                }
                first = false;
                continue;
            }

            if (!matched) {
                std::ostringstream oss;
                oss << "ChainedRuleList: KeySet is not definitely matched. Some previous rules matched but an "
                       "intermediate rule failed: ";
               
                int i = 0;
                for (const auto& r: rules) {
                    if (&r == &rule) {
                        oss << " #" << i << " failed: " << *r.get() << std::endl;
                        break;
                    } else {
                        oss << " #" << i << " matched: " << *r.get() << std::endl;
                    }
                }
                oss << " Keys: " << kvs;
                throw EncodeMtg2Exception(oss.str(), Here());
            }
        }

        return true;
    }
};


template <typename Rule_, typename... Rules_>
ChainedRuleList<typename Rule_::MatchKeySet> chainedRuleList(Rule_&& rule, Rules_&&... rules) {
    using MatchKS = typename Rule_::MatchKeySet;
    ChainedRuleList<MatchKS> res;
    res.rules.emplace_back(std::make_unique<std::decay_t<Rule_>>(std::forward<Rule_>(rule)));
    (res.rules.emplace_back(std::make_unique<std::decay_t<Rules_>>(std::forward<Rules_>(rules))), ...);
    return res;
}


template <typename MatchKeySet>
std::ostream& operator<<(std::ostream& os, const ChainedRuleList<MatchKeySet>& r) {
    os << "exclusiveRuleList(";
    bool first = true;
    for (const auto& ri : r.rules) {
        if (first) {
            first = false;
        }
        else {
            os << ", ";
        }
        os << *ri.get();
    }
    return os;
}


}  // namespace multio::action::rules

