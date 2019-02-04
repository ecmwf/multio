
#ifndef multio_sandbox_DummyTransport_H
#define multio_sandbox_DummyTransport_H

#include <queue>

#include "multio/sandbox/Message.h"

#include "Transport.h"
#include "multio/server/Message.h"

namespace multio {
namespace sandbox {

class DummyTransport final : public Transport {
public:
    DummyTransport(const std::string& title);
    ~DummyTransport() override;

private:
    void receive(Message& msg) override;
    void send(const Message& message) override;

    void print(std::ostream &os) const override;

private:
    std::queue<Message> internalBuffer_;

};

}  // namespace sandbox
}  // namespace multio

#endif
