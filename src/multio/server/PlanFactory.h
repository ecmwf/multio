
#ifndef multio_server_PlanFactory_H
#define multio_server_PlanFactory_H

#include <string>
#include <unordered_map>
#include <vector>

#include "eckit/config/YAMLConfiguration.h"

namespace multio {
namespace server {

class Message;
class Plan;

class PlanFactory {
public:
    PlanFactory();
    bool tryCreate(const Message& msg);
    Plan handOver(const std::string& plan_name);

private:
    eckit::YAMLConfiguration planConfigs_;
    std::unordered_map<std::string, std::vector<std::vector<int>>> plansBeingProcessed_;

private:
    bool isComplete(const std::string& plan_name) const;
};

}  // namespace server
}  // namespace multio

#endif
