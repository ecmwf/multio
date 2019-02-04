
#ifndef multio_sandbox_ThreadTransport_H
#define multio_sandbox_ThreadTransport_H

#include <queue>

#include "multio/sandbox/Message.h"

#include "Transport.h"
#include "multio/server/Message.h"

namespace multio {
namespace sandbox {

class ThreadTransport final : public Transport {
public:
    ThreadTransport(const std::string& title);
    ~ThreadTransport() override;

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
