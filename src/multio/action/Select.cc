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

#include "multio/LibMultio.h"

namespace multio {
namespace action {

namespace {
std::vector<std::string> fetch_items(const std::string& match, const eckit::Configuration& config) {
    return (match == "category") ? config.getStringVector("categories")
                                 : config.getStringVector("fields");
}
}  // namespace

Select::Select(const eckit::Configuration& config) :
    Action{config}, match_{config.getString("match")}, items_{fetch_items(match_, config)} {}

void Select::execute(Message msg) const {
    ScopedTimer timer{timing_};

    LOG_DEBUG_LIB(LibMultio) << " *** Executing aggregation " << *this << std::endl;

    if (isMatched(msg)) {
        executeNext(msg);
    }
}

bool Select::isMatched(const Message& msg) const {
    return (msg.tag() != Message::Tag::Field) || matchPlan(msg);
}

bool Select::matchPlan(const Message& msg) const {
    auto item = (match_ == "category") ? msg.category() : msg.name();

    eckit::Log::debug<LibMultio>()
        << " *** Item " << item << " is being matched...  field size: " << msg.globalSize()
        << std::endl;

    auto it = find(begin(items_), end(items_), item);
    return it != end(items_);
}

void Select::print(std::ostream& os) const {
    os << "Select(categories=";
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
