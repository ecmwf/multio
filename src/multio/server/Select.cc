
#include "Select.h"

#include "atlas/util/Metadata.h"

#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Select::Select(const std::string& name, const std::string& nm) : Action{nm}, plan_name_(name) {}

bool Select::doExecute(std::shared_ptr<Message> msg) const {
    

    auto plan_name = fetch_metadata(*msg).get<std::string>("plan_name");
    return plan_name_ == plan_name;
}

}  // namespace server
}  // namespace multio
