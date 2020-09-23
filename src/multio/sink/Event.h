
#ifndef multio_Event_H
#define multio_Event_H

#include <string>

#include "eckit/types/Types.h"

namespace eckit {
class JSON;
}

namespace  multio {

class Event {

public: // methods

    Event(const std::string& event, eckit::StringDict info = eckit::StringDict{});

    virtual ~Event() = default;

    virtual void json(eckit::JSON& s) const = 0;

    void info(const std::string& k, const std::string& v);

protected:

    std::string type_;

    eckit::StringDict metadata_;
    eckit::StringDict info_;

private:
    virtual void print(std::ostream& o) const = 0;

    friend std::ostream& operator<<(std::ostream& s, const Event& o);
};

//----------------------------------------------------------------------------------------------------------------------

class MetadataChange : public Event {
public:  // methods
    static const char* eventType();

    MetadataChange(eckit::StringDict info = eckit::StringDict{},
                   eckit::StringDict metadata = eckit::StringDict{});
    void metadata(const std::string& k, const std::string& v);
    void json(eckit::JSON &s) const override;

private: // methods

    void print(std::ostream& o) const override;

    eckit::StringDict metadata_;
};

//----------------------------------------------------------------------------------------------------------------------

class NotifyMetadata : public Event {

public: // methods
    static const char* eventType();

    NotifyMetadata(eckit::StringDict info = eckit::StringDict(),
                   eckit::StringDict metadata = eckit::StringDict());
    void metadata(const std::string& k, const std::string& v);
    void json(eckit::JSON& s) const override;

private: // methods
    void print(std::ostream& o) const override;

    eckit::StringDict metadata_;
};

//----------------------------------------------------------------------------------------------------------------------

}

#endif // multio_Event_H
