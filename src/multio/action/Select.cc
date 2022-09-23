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

Select::Select(const ConfigurationContext& confCtx) :
    Action{confCtx},
    match_{confCtx.config().getString("match")},
    items_{fetch_items(match_, confCtx.config())} {
    if (std::none_of(begin(matchTypes), end(matchTypes),
                     [this](const std::string& mt) { return match_ == mt; })) {
        throw eckit::SeriousBug{"Cannot match " + match_};
    }
}

void Select::execute(Message msg) const {
    if (matchPlan(msg)) {
        executeNext(std::move(msg));
    }
}

void Select::activeFields(std::insert_iterator<std::set<std::string>>& ins) const {
    if (match_ == "field") {
        std::copy(items_.begin(), items_.end(), ins);
    }
}
void Select::activeCategories(std::insert_iterator<std::set<std::string>>& ins) const {
    if (match_ == "category") {
        std::copy(items_.begin(), items_.end(), ins);
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
    for (const auto& cat : items_) {
        os << (first ? "" : ", ");
        os << cat;
        first = false;
    }
    os << ")";
}


static ActionBuilder<Select> SelectBuilder("select");

}  // namespace action
}  // namespace multio
