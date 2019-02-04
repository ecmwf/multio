
#ifndef multio_sandbox_Listener_H
#define multio_sandbox_Listener_H

#include <list>

#include "eckit/container/Queue.h"

#include "multio/sandbox/Transport.h"

namespace multio {
namespace sandbox {

struct Connection {
    Connection(int id) : id_(id) {}
    int id_;
};
bool operator==(const Connection& lhs, const Connection& rhs) {
    return lhs.id_ == rhs.id_;
}

class Listener {
public:
    Listener(Transport& trans);

    void listen();

private:
    Transport& transport_;

    std::list<Connection> connections_;
    eckit::Queue<std::shared_ptr<Message>> msgQueue_{1024};
};

}  // namespace sandbox
}  // namespace multio

#endif
