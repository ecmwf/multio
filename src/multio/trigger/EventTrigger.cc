
#include "EventTrigger.h"

#include <algorithm>
#include <fstream>
#include <sstream>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/JSON.h"
#include "eckit/log/Log.h"
#include "eckit/message/Message.h"
#include "eckit/net/TCPClient.h"

#include "multio/LibMultio.h"
#include "multio/trigger/Event.h"

namespace multio {

EventTrigger::EventTrigger(const eckit::Configuration& config) {
    if(config.has("file"))
       file_ = config.getString("file");

    if(config.has("host"))
        host_ = config.getString("host");

    failOnRetry_ = config.getInt("failOnRetry", false);

    port_    = config.getInt("port", 10000);
    retries_ = config.getInt("retries", 5);
    timeout_ = config.getInt("timeout", 60);

    info_["app"] = "multio";
    if(config.has("info"))
    {
        eckit::LocalConfiguration info = config.getSubConfiguration("info");
        std::vector<std::string> keys = info.keys();
        for(std::vector<std::string>::const_iterator it = keys.begin(); it != keys.end(); ++it) {
            info_[*it] = info.getString(*it);
        }
    }
}

EventTrigger* EventTrigger::build(const eckit::Configuration& config) {

    std::string type = config.getString("type");

    if(type == "MetadataChange") {
        return new MetadataChangeTrigger(config);
    }

    if(type == "NotifyMetadata") {
        return new NotifyMetadataTrigger(config);
    }

    throw eckit::BadValue(std::string("Unknown event type ") + type, Here());
}

void EventTrigger::send(const Event &event) const {
    std::ostringstream os;
    eckit::JSON msg(os);
    event.json(msg);

    eckit::Log::info() << "SENDING EVENT -- " << os.str() << std::endl;
    LOG_DEBUG_LIB(LibMultio) << "SENDING EVENT -- " << os.str() << std::endl;

    if(!host_.empty()) {
        try {
            eckit::net::TCPClient c;
            c.connect(host_, port_, retries_, timeout_); // 5 retries, 60 secs timeout
            c.write(os.str().c_str(), os.str().size());
        } catch (eckit::TooManyRetries& e) {
            if(failOnRetry_) {
                eckit::Log::error()
                    << "Failed to send Event " << event << " by TCP connection to host " << host_
                    << ":" << port_ << "-- Exception: " << e.what() << std::endl;
                throw;
            }
        }
    }

    /// this is for unit tests
    if(!file_.empty()) {
        std::ofstream f(eckit::PathName(file_).asString().c_str(), std::ios::app);
        f << os.str() << std::endl;
        f.close();
    }
}

//----------------------------------------------------------------------------------------------------------------------

MetadataChangeTrigger::MetadataChangeTrigger(const eckit::Configuration& config) :
    EventTrigger{config},
    key_(config.getString("key")),
    values_(config.getStringVector("values")),
    lastSeen_(values_.end()),
    issued_(values_.end()) {
    eckit::Log::info() << "Config: " << config << std::endl;
}

MetadataChangeTrigger::~MetadataChangeTrigger() {
    updateEventsIssued();
}

void MetadataChangeTrigger::trigger(const eckit::StringDict& /*msg*/) const {}

void MetadataChangeTrigger::trigger(const eckit::message::Message msg) const {
    std::string current = msg.getString(key_);

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

bool MetadataChangeTrigger::inValues(const std::vector<std::string>::const_iterator& it) const {
    return it != values_.end();
}

/// Updates the events already issued
void MetadataChangeTrigger::updateEventsIssued() const {
    if(issued_ == values_.end() && lastSeen_ != values_.end()) { /* initial */
        issued_ = values_.begin();
        issueEvent(issued_);
    }

    while(issued_ != lastSeen_) {
        ++issued_;
        issueEvent(issued_);
    }
}

void MetadataChangeTrigger::issueEvent(std::vector<std::string>::const_iterator it) const {
    ASSERT(it != values_.end());
    std::string value = *it;

    MetadataChange event(info_);
    event.metadata(key_, value);
    send(event);
}

//----------------------------------------------------------------------------------------------------------------------

NotifyMetadataTrigger::NotifyMetadataTrigger(const eckit::Configuration& config) :
    EventTrigger(config), key_(config.getString("key")) {}


void NotifyMetadataTrigger::trigger(const eckit::StringDict& keys) const {
    eckit::StringDict::const_iterator k = keys.find(key_);
    if(k != keys.end()) {
        NotifyMetadata event(info_);
        event.metadata(key_, k->second);
        send(event);
    }
}

void NotifyMetadataTrigger::trigger(eckit::message::Message) const {}

}  // namespace multio
