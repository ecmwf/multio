
#include "Plan.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/msg_tag.h"
#include "multio/server/Message.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Plan::Plan(const std::string nm, ActionList&& actions) :
    name_(std::move(nm)),
    actions_(std::move(actions)) {}

void Plan::process(const Message& msg) const {
    ASSERT(msg.tag() != msg_tag::plan_data);
    auto field = unpack_atlas_field(msg);

    auto it = begin(actions_);
    do {
        (*it)->execute(field, msg.peer());
    } while (moveOntoNextAction(it, field));
}

bool Plan::moveOntoNextAction(ActionListIt& it, atlas::Field& field) const {
    return (*it)->complete(field) && ++it != end(actions_);
}

}  // namespace server
}  // namespace multio
