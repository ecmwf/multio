
#ifndef multio_sandbox_ThreadTransport_H
#define multio_sandbox_ThreadTransport_H

#include "eckit/container/Queue.h"

#include "multio/sandbox/Message.h"

#include "Transport.h"

namespace multio {
namespace sandbox {

class ThreadTransport final : public Transport {
public:
    ThreadTransport(const eckit::LocalConfiguration& config);
    ~ThreadTransport() override;

private:
    void receive(Message& msg) override;
    void send(const Message& message) override;

    void print(std::ostream &os) const override;

private:
    eckit::Queue<Message> internalBuffer_{1024};

};

}  // namespace sandbox
}  // namespace multio

#endif
