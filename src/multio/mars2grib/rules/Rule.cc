/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "Rule.h"
#include "multio/mars2grib/Grib2Layout.h"
#include "multio/util/Print.h"

namespace multio::mars2grib::rules {


// Match and set
bool ExclusiveRuleList::operator()(const dm::FullMarsRecord& rec, const dm::MiscRecord& misc, LegacySectionsConf& conf,
                                   Grib2Layout& g2l) const {
    DynRule* appliedRule = nullptr;
    for (const auto& rule : rules) {
        if (rule->apply(rec, misc, conf, g2l)) {
            if (appliedRule != nullptr) {
                std::ostringstream oss;
                util::PrintStream ps(oss);
                ps << "ExclusizeRuleList: Multiple rules apply although they should be exclusive." << std::endl;
                ps << " first match: " << std::endl;
                ;
                {
                    util::IndentGuard g(ps);
                    ps << *appliedRule << std::endl;
                }
                ps << " second match: " << std::endl;
                {
                    util::IndentGuard g(ps);
                    ps << *rule.get() << std::endl;
                }
                ps << " Keys: " << std::endl;
                {
                    util::IndentGuard g(ps);
                    ps << rec << std::endl;
                }
                throw Mars2GribException(oss.str(), Here());
            }
            appliedRule = rule.get();
        }
    }
    return (appliedRule != nullptr);
}


ExclusiveRuleList mergeRuleList(ExclusiveRuleList&& res) {
    return std::move(res);
}


}  // namespace multio::mars2grib::rules

namespace multio::util {
void Print<mars2grib::rules::ExclusiveRuleList>::print(PrintStream& ps, const mars2grib::rules::ExclusiveRuleList& r) {
    constexpr int MAX_RULES = 3;
    ps << "exclusiveRuleList(";
    ps.softBreak();
    bool first = true;
    int i = 0;
    for (const auto& ri : r.rules) {
        if (i < MAX_RULES || i > ((int)r.rules.size() - MAX_RULES)) {
            if (first) {
                first = false;
                ps << "  ";
            }
            else {
                ps << ", ";
            }
            {
                IndentGuard g(ps);
                ps << *ri.get();
                ps.softBreak();
            }
        }
        else if (i == MAX_RULES) {
            ps << "  ... " << (((int)r.rules.size()) - ((int)(MAX_RULES * 2))) << " more rules ...";
            ps.softBreak();
        }
        ++i;
    }
    ps << ")";
}
}  // namespace multio::util

namespace multio::mars2grib::rules {


// Match and set
bool ChainedRuleList::operator()(const dm::FullMarsRecord& rec, const dm::MiscRecord& misc, LegacySectionsConf& conf,
                                 Grib2Layout& g2l) const {
    bool first = true;
    for (const auto& rule : rules) {
        bool matched = rule->apply(rec, misc, conf, g2l);
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
            util::PrintStream ps(oss);
            ps << "ChainedRuleList: Some previous rules matched but an intermediate rule failed: " << std::endl;

            int i = 0;
            for (const auto& r : rules) {
                if (&r == &rule) {
                    ps << " #" << i << " failed: " << std::endl;
                    {
                        util::IndentGuard g(ps);
                        ps << *r.get() << std::endl;
                        ;
                    }
                    break;
                }
                else {
                    ps << " #" << i << " matched: " << std::endl;
                    {
                        util::IndentGuard g(ps);
                        ps << *r.get() << std::endl;
                        ;
                    }
                }
                ++i;
            }
            ps << " Keys: " << std::endl;
            {
                util::IndentGuard g(ps);
                util::print(ps, rec);
            }
            throw Mars2GribException(oss.str(), Here());
        }
    }

    return true;
}


}  // namespace multio::mars2grib::rules


namespace multio::util {


void Print<mars2grib::rules::ChainedRuleList>::print(PrintStream& ps, const mars2grib::rules::ChainedRuleList& r) {
    constexpr int MAX_RULES = 3;
    ps << "chainedRuleList(";
    ps.softBreak();
    bool first = true;
    int i = 0;
    for (const auto& ri : r.rules) {
        if ((i < MAX_RULES) || (i > ((int)r.rules.size() - MAX_RULES))) {
            if (first) {
                first = false;
                ps << "  ";
            }
            else {
                ps << ", ";
            }
            {
                IndentGuard g(ps);
                ps << *ri.get();
                ps.softBreak();
            }
        }
        else if (i == MAX_RULES) {
            ps << "   ... " << (((int)r.rules.size()) - ((int)(MAX_RULES * 2))) << " more rules ...";
            ps.softBreak();
        }
        ++i;
    }
    ps << ")";
}


}  // namespace multio::util

