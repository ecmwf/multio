
#include "Action.h"

#include <iostream>

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace server {

Action::Action(const std::string& nm) : name_{nm} {
    ASSERT(!name_.empty());
}

Action* Action::add(std::unique_ptr<Action>&& action) {
    next_ = std::move(action);
    return next_.get();
}

bool Action::execute(atlas::Field& field, const int source) const {
    auto ret = doExecute(field, source);
    if (ret && next_) {
        ret = next_->execute(field, source);
    }
    return ret;
}

void Action::print(std::ostream& os) const {
    os << "Action(=" << name_ << ")" << std::endl;
}

std::ostream& operator<<(std::ostream& os, const Action& action) {
    action.print(os);
    return os;
}

}  // namespace server
}  // namespace multio
