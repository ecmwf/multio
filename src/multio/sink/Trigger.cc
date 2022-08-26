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
#include <fstream>
#include <iosfwd>

#include "multio/sink/Trigger.h"

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/JSON.h"
#include "eckit/net/TCPClient.h"
#include "eckit/types/Types.h"

#include "multio/LibMultio.h"

using namespace eckit;
using namespace eckit::net;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class Event {
protected:  // members
    std::string type_;

    StringDict metadata_;
    StringDict info_;

public:  // methods
    Event(const std::string& event, StringDict info = StringDict()) : type_(event), info_(info) {}

    virtual ~Event() {}

    void info(const std::string& k, const std::string& v) { info_[k] = v; }

    virtual void json(JSON& s) const = 0;

private:  // methods
    virtual void print(std::ostream& o) const = 0;

    friend std::ostream& operator<<(std::ostream& s, const Event& o) {
        o.print(s);
        return s;
    }
};

//----------------------------------------------------------------------------------------------------------------------

class MetadataChange : public Event {
    StringDict metadata_;

public:  // methods
    static const char* eventType() { return "MetadataChange"; }

    MetadataChange(StringDict info = StringDict(), StringDict metadata = StringDict()) :
        Event(eventType(), info), metadata_(metadata) {}

    void metadata(const std::string& k, const std::string& v) { metadata_[k] = v; }

    virtual void json(JSON& s) const override {
        s.startObject();
        s << "type" << type_;
        s << "info" << info_;
        s << "metadata" << metadata_;
        s.endObject();
    }

private:  // methods
    void print(std::ostream& o) const override {
        o << eventType() << "("
          << "type=" << type_ << ",info=" << info_ << ",metadata=" << metadata_ << ")";
    }
};

//----------------------------------------------------------------------------------------------------------------------

class NotifyMetadata : public Event {
    StringDict metadata_;

public:  // methods
    static const char* eventType() { return "NotifyMetadata"; }

    NotifyMetadata(StringDict info = StringDict(), StringDict metadata = StringDict()) :
        Event(eventType(), info), metadata_(metadata) {}

    void metadata(const std::string& k, const std::string& v) { metadata_[k] = v; }

    virtual void json(JSON& s) const override {
        s.startObject();
        s << "type" << type_;
        s << "info" << info_;
        s << "metadata" << metadata_;
        s.endObject();
    }

private:  // methods
    void print(std::ostream& o) const override {
        o << eventType() << "("
          << "type=" << type_ << ",info=" << info_ << ",metadata=" << metadata_ << ")";
    }
};

//----------------------------------------------------------------------------------------------------------------------

class EventTrigger {
public:  // methods
    EventTrigger(const ConfigurationContext& confCtx) {
        if (confCtx.config().has("file"))
            file_ = confCtx.config().getString("file");

        if (confCtx.config().has("host"))
            host_ = confCtx.config().getString("host");

        failOnRetry_ = confCtx.config().getInt("failOnRetry", false);

        port_ = confCtx.config().getInt("port", 10000);
        retries_ = confCtx.config().getInt("retries", 5);
        timeout_ = confCtx.config().getInt("timeout", 60);

        info_["app"] = "multio";
        if (confCtx.config().has("info")) {
            LocalConfiguration info = confCtx.config().getSubConfiguration("info");
            std::vector<std::string> keys = info.keys();
            for (std::vector<std::string>::const_iterator it = keys.begin(); it != keys.end();
                 ++it) {
                info_[*it] = info.getString(*it);
            }
        }
    }

    void send(const Event& event) const {
        std::ostringstream os;
        JSON msg(os);
        event.json(msg);

        Log::info() << "SENDING EVENT -- " << os.str() << std::endl;
        LOG_DEBUG_LIB(LibMultio) << "SENDING EVENT -- " << os.str() << std::endl;

        if (!host_.empty()) {
            try {
                net::TCPClient c;
                c.connect(host_, port_, retries_, timeout_);  // 5 retries, 60 secs timeout
                c.write(os.str().c_str(), os.str().size());
            }
            catch (eckit::TooManyRetries& e) {
                if (failOnRetry_) {
                    Log::error() << "Failed to send Event " << event
                                 << " by TCP connection to host " << host_ << ":" << port_
                                 << "-- Exception: " << e.what() << std::endl;
                    throw;
                }
            }
        }

        /// this is for unit tests
        if (!file_.empty()) {
            std::ofstream f(PathName(file_).asString().c_str(), std::ios::app);
            f << os.str() << std::endl;
            f.close();
        }
    }

    virtual ~EventTrigger() {}

    virtual void trigger(const StringDict& keys) const = 0;
    virtual void trigger(eckit::message::Message msg) const = 0;

    static EventTrigger* build(const ConfigurationContext& config);

protected:  // member
    int port_;
    int retries_;
    int timeout_;

    std::string host_;

    std::string file_;

    StringDict info_;

    bool failOnRetry_;
};

//----------------------------------------------------------------------------------------------------------------------

class MetadataChangeTrigger : public EventTrigger {
public:  // methods
    explicit MetadataChangeTrigger(const ConfigurationContext& confCtx) :
        EventTrigger(confCtx),
        key_(confCtx.config().getString("key")),
        values_(confCtx.config().getStringVector("values")),
        lastSeen_(values_.end()),
        issued_(values_.end()) {
    }

    ~MetadataChangeTrigger() override { updateEventsIssued(); }

    virtual void trigger(const StringDict&) const override {}

    virtual void trigger(eckit::message::Message msg) const override {
        std::string current = msg.getString(key_);

        std::vector<std::string>::const_iterator now =
            std::find(values_.begin(), values_.end(), current);

        if (lastSeen_ == values_.end() && inValues(now)) { /* initial */
            lastSeen_ = now;
        }

        if (lastSeen_ == now)
            return; /* no change => no event */

        updateEventsIssued();

        if (inValues(now)) {
            lastSeen_ = now;
        }
    }


private:  // methods
    bool inValues(const std::vector<std::string>::const_iterator& it) const {
        return it != values_.end();
    }

    /// Updates the events already issued
    void updateEventsIssued() const {
        if (issued_ == values_.end() && lastSeen_ != values_.end()) { /* initial */
            issued_ = values_.begin();
            issueEvent(issued_);
        }

        while (issued_ != lastSeen_) {
            ++issued_;
            issueEvent(issued_);
        }
    }

    void issueEvent(std::vector<std::string>::const_iterator it) const {
        ASSERT(it != values_.end());
        std::string value = *it;

        MetadataChange event(info_);
        event.metadata(key_, value);
        send(event);
    }


private:  // members
    std::string key_;
    std::vector<std::string> values_;

    mutable std::vector<std::string>::const_iterator lastSeen_;
    mutable std::vector<std::string>::const_iterator issued_;
};


class NotifyMetadataTrigger : public EventTrigger {
public:  // methods
    NotifyMetadataTrigger(const ConfigurationContext& confCtx) :
        EventTrigger(confCtx), key_(confCtx.config().getString("key")) {}


    ~NotifyMetadataTrigger() {}

    virtual void trigger(const eckit::StringDict& keys) const override {
        eckit::StringDict::const_iterator k = keys.find(key_);
        if (k != keys.end()) {
            NotifyMetadata event(info_);
            event.metadata(key_, k->second);
            send(event);
        }
    }

    virtual void trigger(eckit::message::Message) const override {}

private:
    std::string key_;
};

//----------------------------------------------------------------------------------------------------------------------

EventTrigger* EventTrigger::build(const ConfigurationContext& confCtx) {
    std::string type = confCtx.config().getString("type");

    if (type == "MetadataChange") {
        return new MetadataChangeTrigger(confCtx);
    }

    if (type == "NotifyMetadata") {
        return new NotifyMetadataTrigger(confCtx);
    }

    throw eckit::BadValue(std::string("Unknown event type ") + type, Here());
}


//----------------------------------------------------------------------------------------------------------------------

Trigger::Trigger(const ConfigurationContext& confCtx) {
    if (confCtx.config().has("triggers")) {
        for (auto&& subCtx: confCtx.subContexts("triggers")) {
            triggers_.push_back(EventTrigger::build(std::move(subCtx)));
        }
    }

    /// @note this doesn't quite work for reentrant MultIO objects (MultIO as a DataSink itself)
    const char* conf = ::getenv("MULTIO_CONFIG_TRIGGERS");
    if (conf) {
        std::string confString(conf);
        ConfigurationContext confCtx2(eckit::LocalConfiguration(eckit::YAMLConfiguration(confString)), confCtx.pathName(), confString);

        for (auto&& subCtx: confCtx2.subContexts("triggers")) {
            triggers_.push_back(EventTrigger::build(std::move(subCtx)));
        }
    }
}

Trigger::~Trigger() {
    for (std::vector<EventTrigger*>::iterator it = triggers_.begin(); it != triggers_.end(); ++it) {
        delete *it;
    }
}

void Trigger::events(const StringDict& keys) const {
    for (std::vector<EventTrigger*>::const_iterator it = triggers_.begin(); it != triggers_.end();
         ++it) {
        (*it)->trigger(keys);
    }
}

void Trigger::events(eckit::message::Message msg) const {
    for (std::vector<EventTrigger*>::const_iterator it = triggers_.begin(); it != triggers_.end();
         ++it) {
        (*it)->trigger(msg);
    }
}

void Trigger::print(std::ostream& os) const {
    os << "Trigger()";
}

}  // namespace multio

