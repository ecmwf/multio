/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Select.h"

#include <algorithm>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace action {

namespace {
const std::initializer_list<std::string> matchTypes = {"category", "field"};

const std::map<std::string, std::string> plural = {{"category", "categories"}, {"field", "fields"}};

const std::map<std::string, std::string> mdEntry = {{"category", "category"}, {"field", "name"}};

std::vector<std::string> fetch_items(const std::string& match, const eckit::Configuration& config) {
    return config.getStringVector(plural.at(match));
}
}  // namespace

Select::Select(const eckit::Configuration& config) :
    Action{config}, match_{config.getString("match")}, items_{fetch_items(match_, config)} {
    if (std::none_of(begin(matchTypes), end(matchTypes),
                     [this](const std::string& mt) { return match_ == mt; })) {
        throw eckit::SeriousBug{"Cannot match " + match_};
    }
}

void Select::execute(Message msg) const {
    if (matchPlan(msg)) {
        executeNext(msg);
    }
}

bool Select::matchPlan(const Message& msg) const {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
    auto item = msg.metadata().getString(mdEntry.at(match_));

    LOG_DEBUG_LIB(LibMultio) << " *** Item " << item << " is being matched... ";

    bool ret = find(begin(items_), end(items_), item) != end(items_);

    LOG_DEBUG_LIB(LibMultio) << (ret ? "found" : "not found") << std::endl;

    return ret;
}

void Select::print(std::ostream& os) const {
    os << "Select(" << plural.at(match_) << "=(";
    bool first = true;
    for(const auto& cat : items_) {
        os << (first ? "" : ", ");
        os << cat;
        first = false;
    }
    os << ")";
}


static ActionBuilder<Select> SelectBuilder("Select");

}  // namespace action
}  // namespace multio
