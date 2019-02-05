
#ifndef multio_sandbox_ThreadTransport_H
#define multio_sandbox_ThreadTransport_H

#include <map>
#include <thread>

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

private:
    void addNewQueueIfNeeded(std::thread::id server_id);

private:
    std::map<std::thread::id, eckit::Queue<Message>> internalBuffers_;
    size_t no_servers;

};

}  // namespace sandbox
}  // namespace multio

#endif
