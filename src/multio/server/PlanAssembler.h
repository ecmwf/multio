
#ifndef multio_server_PlanAssembler_H
#define multio_server_PlanAssembler_H

#include <string>
#include <unordered_map>
#include <vector>

#include "eckit/config/YAMLConfiguration.h"

namespace multio {
namespace server {

class Action;
class Message;
class Plan;

class PlanAssembler {
public:
    PlanAssembler();
    bool tryCreate(const Message& msg);
    std::unique_ptr<Action> handOver(const std::string& plan_name);

private:
    eckit::YAMLConfiguration planConfigs_;
    std::unordered_map<std::string, std::vector<std::vector<int>>> plansBeingProcessed_;

private:
    bool isComplete(const std::string& plan_name) const;
};

}  // namespace server
}  // namespace multio

#endif
