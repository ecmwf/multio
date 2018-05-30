/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <iosfwd>
#include <fstream>
#include <algorithm>

#include "multio/Trigger.h"

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/types/Metadata.h"
#include "eckit/net/TCPClient.h"
#include "eckit/parser/JSON.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class Event {

    std::string event_;
    std::string app_;

    StringDict metadata_;

public: // methods

    Event(const std::string& event, const std::string& app) :
        event_(event),
        app_(app) {
    }

    void set(const std::string& k, const std::string& v) { metadata_[k] = v; }

    void json(JSON& s) const {
        s.startObject();
        s << "event" << event_;
        s << "app"  << app_;
        for(StringDict::const_iterator it = metadata_.begin(); it != metadata_.end(); ++it) {
            s << it->first << it->second;
        }
        s.endObject();
    }
};

//----------------------------------------------------------------------------------------------------------------------

class EventTrigger {

public: // methods

    EventTrigger(const Configuration& config) {}

    virtual ~EventTrigger() {}

    virtual void trigger(eckit::DataBlobPtr blob) const = 0;

    static EventTrigger* build(const Configuration& config);
};

//----------------------------------------------------------------------------------------------------------------------

class MetadataChangeTrigger : public EventTrigger {

public: // methods

    static const char* eventType() { return "MetadataChange"; }

    MetadataChangeTrigger(const Configuration& config) : EventTrigger(config),
        key_(config.getString("key")),
        values_(config.getStringVector("values")),
        lastSeen_(values_.end()),
        issued_(values_.end())
    {
        ASSERT(config.getString("type") == eventType());

        if(config.has("port"))
            port_ = config.getInt("port");

        if(config.has("host"))
            host_ = config.getString("host");

    }


    ~MetadataChangeTrigger() {
        updateEventsIssued();
    }

    virtual void trigger(eckit::DataBlobPtr blob) const {

        const eckit::Metadata& md = blob->metadata();

        std::string current;
        md.get(key_, current);

        std::vector<std::string>::const_iterator now = std::find(values_.begin(), values_.end(), current);

        if(lastSeen_ == values_.end() && inValues(now)) { /* initial */
            lastSeen_ = now;
        }

        if(lastSeen_ == now) return; /* no change => no event */

        updateEventsIssued();

        if(inValues(now)) {
            lastSeen_ = now;
        }
    }

private: // methods


    bool inValues(const std::vector<std::string>::const_iterator& it) const { return it != values_.end(); }

    /// Updates the events already issued
    void updateEventsIssued() const {

        if(issued_ == values_.end() && lastSeen_ != values_.end()) { /* initial */
            issued_ = values_.begin();
            issueEvent(issued_);
        }

        while(issued_ != lastSeen_) {
            issued_++;
            issueEvent(issued_);
        }
    }

    void issueEvent(std::vector<std::string>::const_iterator it) const {
        ASSERT(it != values_.end());
        std::string value = *it;

        Log::info() << "EVENT ISSUED -- Key " << key_ << " Value " << value << std::endl;

        Event e(eventType(), "multio");
        e.set(key_, value);

        std::ostringstream os;
        JSON msg(os);

        if(!host_.empty()) {
            TCPClient c;
            c.connect(host_, port_);
            c.write(os.str().c_str(), os.str().size());
        }
    }


private: // members

    int port_;
    std::string host_;

    std::string key_;
    std::vector<std::string> values_;

    mutable std::vector<std::string>::const_iterator lastSeen_;
    mutable std::vector<std::string>::const_iterator issued_;

};

//----------------------------------------------------------------------------------------------------------------------


EventTrigger* EventTrigger::build(const Configuration& config) {

    std::string type = config.getString("type");

    ASSERT(type == "MetadataChange");

    return new MetadataChangeTrigger(config);
}


//----------------------------------------------------------------------------------------------------------------------

Trigger::Trigger(const Configuration& config) {

    std::vector<LocalConfiguration> cfgs = config.getSubConfigurations("triggers");

    for(std::vector<LocalConfiguration>::const_iterator it = cfgs.begin(); it != cfgs.end(); ++it) {
        triggers_.push_back(EventTrigger::build(*it));
    }
}

Trigger::~Trigger() {
    for(std::vector<EventTrigger*>::iterator it = triggers_.begin(); it !=  triggers_.end(); ++it) {
        delete *it;
    }
}

void Trigger::events(eckit::DataBlobPtr blob) {
    for(std::vector<EventTrigger*>::const_iterator it = triggers_.begin(); it !=  triggers_.end(); ++it) {
        (*it)->trigger(blob);
    }
}

void Trigger::print(std::ostream& os) const {
    os << "Trigger()";
}

}  // namespace multio


