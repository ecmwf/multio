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
namespace sandbox {
namespace actions {

Select::Select(const eckit::Configuration& config) :
    Action(config),
    categories_(config.getStringVector("categories")) {}

void Select::execute(Message msg) {
    if (msg.tag() == Message::Tag::Field) {
        ASSERT(matchPlan(msg));
    }
}

bool Select::matchPlan(const Message& msg) const {
    // auto category = fetch_metadata(msg).getString("category");
    auto category = "prognostic";
    auto it = find(begin(categories_), end(categories_), category);
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
}  // namespace sandbox
}  // namespace multio
