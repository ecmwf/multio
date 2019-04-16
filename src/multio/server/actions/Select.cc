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

namespace multio {
namespace server {
namespace actions {

Select::Select(const eckit::Configuration& config) :
    Action(config),
    categories_(config.getStringVector("categories")) {}

void Select::execute(Message msg) const {
    bool passOn = (msg.tag() != Message::Tag::Field) || matchPlan(msg);

    if (passOn && next_) { // May want to assert next_
        next_->execute(msg);
    }
}

bool Select::matchPlan(const Message& msg) const {
    auto it = find(begin(categories_), end(categories_), msg.category());
    return it != end(categories_);
}

void Select::print(std::ostream& os) const {
    os << "Select(categories=";
    bool first = true;
    for(const auto& cat : categories_) {
        os << (first ? "" : ", ");
        os << cat;
        first = false;
    }
    os << ")";
}


static ActionBuilder<Select> SelectBuilder("Select");

}  // namespace actions
}  // namespace server
}  // namespace multio
