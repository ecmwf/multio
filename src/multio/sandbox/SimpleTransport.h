
#ifndef multio_sandbox_SimpleTransport_H
#define multio_sandbox_SimpleTransport_H

#include "eckit/container/Queue.h"

#include "multio/sandbox/Message.h"

#include "Transport.h"

namespace multio {
namespace sandbox {

class SimpleTransport final : public Transport {
public:
    SimpleTransport(const eckit::Configuration& config);
    ~SimpleTransport() override;

private:
    eckit::Queue<Message> buffer_{1024};

private:
    void receive(Message& msg) override;
    void send(const Message& message) override;

    void print(std::ostream& os) const override;

};

}  // namespace sandbox
}  // namespace multio

#endif
