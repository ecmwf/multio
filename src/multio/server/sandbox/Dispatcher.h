
#ifndef multio_server_sandbox_Dispatcher_H
#define multio_server_sandbox_Dispatcher_H

#include <memory>

#include "eckit/container/Queue.h"

namespace multio {
namespace server {

class Message;

namespace sandbox {

class Dispatcher {
public:
    Dispatcher();

    void dispatch(eckit::Queue<std::shared_ptr<Message>>& queue);
};

}  // namespace sandbox
}  // namespace server
}  // namespace multio

#endif
