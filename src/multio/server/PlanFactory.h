
#ifndef multio_server_PlanFactory_H
#define multio_server_PlanFactory_H

#include <string>
#include <unordered_map>
#include <vector>

namespace multio {
namespace server {

class Message;
class Plan;

class PlanFactory {
public:
    explicit PlanFactory(size_t no_maps);
    bool tryCreate(const Message& msg);
    Plan handOver(const std::string& plan_name);

private:
    const size_t noMaps_;
    std::unordered_map<std::string, std::vector<std::vector<int>>> plansBeingProcessed_;

private:
    bool isComplete(const std::string& plan_name) const;
};

}  // namespace server
}  // namespace multio

#endif
