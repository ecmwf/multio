
#include "Select.h"

#include "atlas/array.h"
#include "atlas/util/Metadata.h"

namespace multio {
namespace server {

Select::Select(const std::string& name, const std::string& nm) : Action{nm}, plan_name_(name) {}

bool Select::doExecute(atlas::Field& field, int source) const {
    return plan_name_ == field.metadata().get<std::string>("plan_name");
}

}  // namespace server
}  // namespace multio
