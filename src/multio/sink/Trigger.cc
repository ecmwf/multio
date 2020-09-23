/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/sink/Trigger.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"

#include "multio/sink/EventTrigger.h"

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
        std::vector<LocalConfiguration> cfgs = config.getSubConfigurations("triggers");

        for(std::vector<LocalConfiguration>::const_iterator it = cfgs.begin(); it != cfgs.end(); ++it) {
            triggers_.push_back(EventTrigger::build(*it));
        }
    }

    /// @note this doesn't quite work for reentrant MultIO objects (MultIO as a DataSink itself)
    const char * conf = ::getenv("MULTIO_CONFIG_TRIGGERS");
    if(conf) {
        eckit::YAMLConfiguration econf((std::string(conf)));

        std::vector<LocalConfiguration> cfgs = econf.getSubConfigurations("triggers");

        for(std::vector<LocalConfiguration>::const_iterator it = cfgs.begin(); it != cfgs.end(); ++it) {
            triggers_.push_back(EventTrigger::build(*it));
        }
    }
}

Trigger::~Trigger() {
    for(std::vector<EventTrigger*>::iterator it = triggers_.begin(); it !=  triggers_.end(); ++it) {
        delete *it;
    }
}

void Trigger::events(const StringDict& keys) const {
    for(std::vector<EventTrigger*>::const_iterator it = triggers_.begin(); it !=  triggers_.end(); ++it) {
        (*it)->trigger(keys);
    }
}

void Trigger::events(eckit::message::Message msg) const {
    for(std::vector<EventTrigger*>::const_iterator it = triggers_.begin(); it !=  triggers_.end(); ++it) {
        (*it)->trigger(msg);
    }
}

void Trigger::print(std::ostream& os) const {
    os << "Trigger()";
}

}  // namespace multio

