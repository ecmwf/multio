
#ifndef multio_sandbox_Transport_H
#define multio_sandbox_Transport_H

#include <string>

#include "eckit/memory/NonCopyable.h"

#include "multio/sandbox/Message.h"

namespace multio {
namespace sandbox {

class Transport : public eckit::NonCopyable {
public: // methods

    Transport(const std::string& title);
    virtual ~Transport();

    virtual void receive(Message &message) = 0;
    virtual void send(const Message &message) = 0;

protected:

    const std::string title_;

private: // methods

    virtual void print(std::ostream &os) const = 0;

    friend std::ostream& operator<<(std::ostream& os, const Transport& transport) {
        transport.print(os);
        return os;
    }
};

}  // namespace sandbox
}  // namespace multio

#endif
