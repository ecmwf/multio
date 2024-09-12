/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "TagFilter.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"

using multio::message::Message;
using multio::message::match::MatchReduce;

namespace multio::action {

//--------------------------------------------------------------------------------------------------

TagFilter::TagFilter(const ComponentConfiguration& compConf) : ChainedAction{compConf}, tags_{} {
    if (compConf.parsedConfig().has("tags")) {
        if (compConf.parsedConfig().isString("tags")) {
            tags_.emplace(message::Message::parseTag(compConf.parsedConfig().getString("tags")));
        } else if (compConf.parsedConfig().isStringList("tags")) {
            for(const auto& t: compConf.parsedConfig().getStringVector("tags")) {
                tags_.emplace(message::Message::parseTag(t));
            }
        } else {
            std::ostringstream oss;
            oss << "TagFilter: Key \"tags\" must be a string or list of strings." << compConf.parsedConfig();
            throw eckit::UserError(oss.str(), Here());
        }
    }
}

void TagFilter::executeImpl(Message msg) {
    // Tag filter is important for parametrization - 
    // otherwise parametrization is made on a custom key "category"... 
    // which will be read for all followup metadata that has no explicit key "Tag"
    if (!tags_.empty()) {
        if (auto search = tags_.find(msg.tag()); search == tags_.end()) {
            // Tag not found
            return;
        }
    }
    
    executeNext(std::move(msg));
}

void TagFilter::print(std::ostream& os) const {
    os << "TagFilter(";
    bool first = true;
    for(auto t: tags_) {
        if (!first) {
            os << ", ";
            first = false;
        }
        os << message::Message::tag2str(t);
    }
}

//--------------------------------------------------------------------------------------------------

static ActionBuilder<TagFilter> TagFilterBuilder("tag-filter");

//--------------------------------------------------------------------------------------------------

}  // namespace multio::action
