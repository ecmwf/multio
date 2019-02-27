
#ifndef multio_server_PlanAssembler_H
#define multio_server_PlanAssembler_H

#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "eckit/config/YAMLConfiguration.h"

namespace atlas {
namespace util {
class Metadata;
}
}  // namespace atlas

namespace multio {
namespace server {

class Action;
class Message;
class Plan;

class PlanAssembler {
public:
    PlanAssembler();
    std::set<Plan> createAllPlans();

private:
    eckit::YAMLConfiguration planConfigs_;

private:

    Plan createPlan(const atlas::util::Metadata& config);
};

}  // namespace server
}  // namespace multio

#endif
