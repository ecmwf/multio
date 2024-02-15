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

using multio::message::Message;
using multio::message::match::MatchReduce;

namespace multio::action {

//--------------------------------------------------------------------------------------------------

Select::Select(const ComponentConfiguration& compConf) : ChainedAction{compConf}, selectors_{compConf.parsedConfig()} {}

void Select::executeImpl(Message msg) {
    if (matches(msg)) {
        executeNext(std::move(msg));
    }
}

bool Select::matches(const Message& msg) const {
    util::ScopedTiming timing{statistics_.actionTiming_};
    return selectors_.matches(msg.metadata());
}

void Select::matchedFields(MatchReduce& selectors) const {
    selectors.extend(selectors_);
}

void Select::print(std::ostream& os) const {
    os << "Select(" << selectors_ << ")";
}

//--------------------------------------------------------------------------------------------------

static ActionBuilder<Select> SelectBuilder("select");

//--------------------------------------------------------------------------------------------------

}  // namespace multio::action
