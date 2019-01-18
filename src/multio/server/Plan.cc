
#include "Plan.h"

#include "eckit/exception/Exceptions.h"

#include "multio/server/msg_tag.h"
#include "multio/server/Message.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Plan::Plan(const std::string& nm, std::unique_ptr<Action>&& root) :
    name_(nm),
    root_(std::move(root)) {}

void Plan::process(std::shared_ptr<Message> msg) const {
    ASSERT(msg->tag() != msg_tag::plan_data);

    // Enque on one thread

    // Deque and execute other potentially more threads

    // Deque

    // Execute
    root_->execute(msg);
}

}  // namespace server
}  // namespace multio
