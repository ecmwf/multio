
#ifndef multio_sandbox_ThreadTransport_H
#define multio_sandbox_ThreadTransport_H

#include <map>
#include <mutex>
#include <thread>

#include "eckit/container/Queue.h"

#include "multio/sandbox/Message.h"

#include "Transport.h"

namespace multio {
namespace sandbox {

class ThreadTransport final : public Transport {
public:
    ThreadTransport(const eckit::Configuration& config);
    ~ThreadTransport() override;

private:
    std::map<Peer, eckit::Queue<Message>> buffers_;
    int no_servers_;
    int no_clients_;

    std::mutex mutex_;

private:
    void receive(Message& msg) override;
    void send(const Message& message) override;

    void print(std::ostream& os) const override;

    void addNewQueueIfNeeded(Peer consumer);

};

}  // namespace sandbox
}  // namespace multio

#endif
