/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */


#include "multio/mars2mars/rules/Rule.h"
#include "eckit/utils/Overloaded.h"


namespace multio::mars2mars::rules {

const DynRule* getRulePtr(const RuleList::RuleEntry& re) {
    return std::visit(
        eckit::Overloaded{[&](const std::unique_ptr<DynRule>& rule) { return static_cast<const DynRule*>(rule.get()); },
                          [&](std::reference_wrapper<const RuleList> ruleList) {
                              return static_cast<const DynRule*>(&ruleList.get());
                          }},
        re);
}

std::optional<MappingResult> RuleList::operator()(dm::MarsRecord& marsVals, dm::MiscRecord& miscVals) const {
    const DynRule* appliedRule = nullptr;
    std::optional<MappingResult> res;
    for (const auto& ruleEntry : rules) {
        const DynRule* rule = getRulePtr(ruleEntry);
        if (auto res2 = rule->apply(marsVals, miscVals)) {
            if (appliedRule != nullptr) {
                std::ostringstream oss;
                util::PrintStream ps(oss);
                ps << "RuleList: Multiple rules apply";

                int i = 0;
                for (const auto& r : rules) {
                    if (&r == &ruleEntry) {
                        ps << " #" << i << " second match: ";
                        {
                            util::IndentGuard g(ps);
                            ps << *getRulePtr(r);
                        }
                        ps << std::endl;
                        break;
                    }
                    else if (appliedRule == getRulePtr(r)) {
                        ps << " #" << i << " first match: ";
                        {
                            util::IndentGuard g(ps);
                            ps << *getRulePtr(r);
                        }
                        ps << std::endl;
                    }
                }
                ps << " Keys: ";
                {
                    util::IndentGuard g(ps);
                    ps << marsVals;
                }
                throw Mars2MarsException(oss.str(), Here());
            }
            appliedRule = rule;
            res = res2;
        }
    }
    return res;
}


}  // namespace multio::mars2mars::rules


namespace multio::util {

void Print<mars2mars::rules::DynRule>::print(PrintStream& ps, const mars2mars::rules::DynRule& r) {
    r.print(ps);
}


void Print<mars2mars::rules::RuleList>::print(PrintStream& ps, const mars2mars::rules::RuleList& r) {
    ps << "ruleList(";
    bool first = true;
    for (const auto& ri : r.rules) {
        if (first) {
            first = false;
            ps << "  ";
        }
        else {
            ps << ", ";
        }
        {
            IndentGuard g(ps);
            getRulePtr(ri)->print(ps);
        }
    }
}


}  // namespace multio::util

