
#ifndef multio_EventTrigger_H
#define multio_EventTrigger_H

#include "eckit/types/Types.h"

namespace eckit {
class Configuration;
namespace message {
class Message;
}
}

namespace multio {

class Event;

class EventTrigger
{
public:
    EventTrigger(const eckit::Configuration& config);
    virtual ~EventTrigger() = default;

    static EventTrigger* build(const eckit::Configuration& config);

    void send(const Event& event) const;

    virtual void trigger(const eckit::StringDict& keys) const = 0;
    virtual void trigger(eckit::message::Message msg) const = 0;

protected:
    int port_;
    int retries_;
    int timeout_;

    std::string host_;
    std::string file_;
    eckit::StringDict info_;

    bool failOnRetry_;
};


class MetadataChangeTrigger : public EventTrigger {

public: // methods

    explicit MetadataChangeTrigger(const eckit::Configuration& config);

    ~MetadataChangeTrigger() override;

    void trigger(const eckit::StringDict&) const override;

    void trigger(eckit::message::Message msg) const override;

private: // methods

    bool inValues(const std::vector<std::string>::const_iterator& it) const;

    /// Updates the events already issued
    void updateEventsIssued() const;

    void issueEvent(std::vector<std::string>::const_iterator it) const;

    std::string key_;
    std::vector<std::string> values_;

    mutable std::vector<std::string>::const_iterator lastSeen_;
    mutable std::vector<std::string>::const_iterator issued_;
};


class NotifyMetadataTrigger : public EventTrigger {

public: // methods

    NotifyMetadataTrigger(const eckit::Configuration& config);

    void trigger(const eckit::StringDict& keys) const override;

    void trigger(eckit::message::Message) const override;

private:

    std::string key_;

};
}  // namespace multio

#endif // multio_EventTrigger_H
