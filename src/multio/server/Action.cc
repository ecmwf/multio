
#include "Action.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include <iostream>

namespace multio {
namespace server {

Action::Action(const std::string& nm) : name_{nm} {
    ASSERT(!name_.empty());
}

void Action::execute(const atlas::Field& field, const int source) const {
    do_execute(field, source);
}

bool Action::complete(atlas::Field& field) const {
    return do_complete(field);
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
