
#ifndef multio_sandbox_Transport_H
#define multio_sandbox_Transport_H

#include <iostream>
#include <string>

#include "eckit/memory/NonCopyable.h"
#include "eckit/config/LocalConfiguration.h"

#include "multio/sandbox/Message.h"

namespace multio {
namespace sandbox {

template <typename ValueType>
ValueType get_config_value(const eckit::LocalConfiguration& config, std::string value_name) {
    ValueType val;
    config.get(value_name, val);
    return val;
}

class Transport {
public:  // methods
    Transport(const eckit::LocalConfiguration& config);
    virtual ~Transport();

    virtual void receive(Message &message) = 0;
    virtual void send(const Message &message) = 0;

protected:

    const eckit::LocalConfiguration& config_;

private: // methods

    void print(std::ostream &os) const;

    friend std::ostream& operator<<(std::ostream& os, const Transport& transport) {
        transport.print(os);
        return os;
    }
};

}  // namespace sandbox
}  // namespace multio

#endif
