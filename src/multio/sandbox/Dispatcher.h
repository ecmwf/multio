
#ifndef multio_sandbox_Dispatcher_H
#define multio_sandbox_Dispatcher_H

#include <memory>

#include "eckit/container/Queue.h"

#include "multio/sandbox/Message.h"

namespace multio {
namespace sandbox {

class Dispatcher {
public:
    Dispatcher();

    void dispatch(eckit::Queue<std::shared_ptr<Message>>& queue);
};

}  // namespace sandbox
}  // namespace multio

#endif
