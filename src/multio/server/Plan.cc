
#include "Plan.h"

#include "eckit/exception/Exceptions.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/msg_tag.h"
#include "multio/server/Message.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Plan::Plan(const std::string& nm, std::unique_ptr<Action>&& root) :
    name_(nm),
    root_(std::move(root)) {}

void Plan::process(const Message& msg) const {
    ASSERT(msg.tag() != msg_tag::plan_data);
    auto field = unpack_atlas_field(msg);

    root_->execute(field, msg.peer());
}

}  // namespace server
}  // namespace multio
