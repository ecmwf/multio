/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <algorithm>

#include "Trigger.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"

#include "multio/trigger/EventTrigger.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

std::ostream& operator<<(std::ostream& s, const Trigger& p) {
    p.print(s);
    return s;
}

Trigger::Trigger(const Configuration& config) {

    if(config.has("triggers"))
    {
        auto cfgs = config.getSubConfigurations("triggers");

        for(const auto& cfg : cfgs) {
            triggers_.emplace_back(EventTrigger::build(cfg));
        }
    }

    /// @note this doesn't quite work for reentrant MultIO objects (MultIO as a DataSink itself)
    const char * conf = ::getenv("MULTIO_CONFIG_TRIGGERS");
    if(conf) {
        eckit::YAMLConfiguration econf{std::string{conf}};

        auto cfgs = econf.getSubConfigurations("triggers");

        for(const auto& cfg : cfgs) {
            triggers_.emplace_back(EventTrigger::build(cfg));
        }
    }
}

Trigger::~Trigger() = default;

void Trigger::events(const StringDict& keys) const {
    for(const auto& trigger : triggers_) {
        trigger->trigger(keys);
    }
}

void Trigger::events(eckit::message::Message msg) const {
    for(const auto& trigger : triggers_) {
        trigger->trigger(msg);
    }
}

void Trigger::print(std::ostream& os) const {
    os << "Trigger()";
}

}  // namespace multio

