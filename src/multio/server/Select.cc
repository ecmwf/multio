
#include "Select.h"

#include "atlas/util/Metadata.h"

#include "eckit/exception/Exceptions.h"

#include "multio/server/Message.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Select::Select(const std::string& name, const std::string& nm) : Action{nm}, plan_name_(name) {}

bool Select::doExecute(std::shared_ptr<Message> msg) const {
    switch (msg->tag()) {
        case msg_tag::field_data:
            return matchPlan(*msg);

        case msg_tag::step_complete:
            return true;

        default:
            ASSERT(false);
            return false;
    }
}

bool Select::matchPlan(const Message& msg) const {
    return plan_name_ == fetch_metadata(msg).get<std::string>("plan_name");
}

}  // namespace server
}  // namespace multio
