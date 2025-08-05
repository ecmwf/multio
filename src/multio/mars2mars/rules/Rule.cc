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


namespace multio::mars2mars::rules {

std::optional<MappingResult> RuleList::operator()(datamod::MarsKeyValueSet& marsVals,
                                                  datamod::MiscKeyValueSet& miscVals) const {
    DynRule* appliedRule = nullptr;
    std::optional<MappingResult> res;
    for (const auto& rule : rules) {
        if (auto res2 = rule->apply(marsVals, miscVals)) {
            if (appliedRule != nullptr) {
                std::ostringstream oss;
                oss << "RuleList: Multiple rules apply";

                int i = 0;
                for (const auto& r : rules) {
                    if (&r == &rule) {
                        oss << " #" << i << " second match: ";
                        util::print(oss, *r.get());
                        oss << std::endl;
                        break;
                    }
                    else if (appliedRule == r.get()) {
                        oss << " #" << i << " first match: ";
                        util::print(oss, *r.get());
                        oss << std::endl;
                    }
                }
                oss << " Keys: ";
                util::print(oss, marsVals);
                throw Mars2MarsException(oss.str(), Here());
            }
            appliedRule = rule.get();
            res = res2;
        }
    }
    return res;
}


}  // namespace multio::mars2mars::rules


namespace multio::util {

void Print<mars2mars::rules::DynRule>::print(std::ostream& os, const mars2mars::rules::DynRule& r) {
    r.print(os);
}


void Print<mars2mars::rules::RuleList>::print(std::ostream& os, const mars2mars::rules::RuleList& r) {
    os << "ruleList(";
    bool first = true;
    for (const auto& ri : r.rules) {
        if (first) {
            first = false;
        }
        else {
            os << ", ";
        }
        util::print(os, *ri.get());
    }
}


}  // namespace multio::util

