
#ifndef multio_server_sandbox_DummyTransport_H
#define multio_server_sandbox_DummyTransport_H

#include <queue>

#include "Transport.h"

namespace multio {
namespace server {
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
}  // namespace server
}  // namespace multio

#endif
