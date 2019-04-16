
#include <algorithm>

#include "Select.h"

#include "atlas/util/Metadata.h"

#include "eckit/exception/Exceptions.h"

#include "multio/attic/Message.h"
#include "multio/attic/print_buffer.h"
#include "multio/attic/SerialisationHelpers.h"

namespace multio {
namespace attic {

Select::Select(const std::vector<std::string>& ctgs, const std::string& nm) :
    Action{nm},
    categories_(ctgs) {}

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
    auto category = fetch_metadata(msg).get<std::string>("category");
    auto it = find(begin(categories_), end(categories_), category);
    return it != end(categories_);
}

}  // namespace attic
}  // namespace multio
