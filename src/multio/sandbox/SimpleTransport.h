
#ifndef multio_sandbox_SimpleTransport_H
#define multio_sandbox_SimpleTransport_H

#include "eckit/container/Queue.h"

#include "multio/sandbox/Message.h"

#include "Transport.h"

namespace multio {
namespace sandbox {

class SimpleTransport final : public Transport {
public:
    SimpleTransport(const eckit::LocalConfiguration& config);
    ~SimpleTransport() override;

private:
    void receive(Message& msg) override;
    void send(const Message& message) override;

private:
    eckit::Queue<Message> internalBuffer_{1024};

};

}  // namespace sandbox
}  // namespace multio

#endif
