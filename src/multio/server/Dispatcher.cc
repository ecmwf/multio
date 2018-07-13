
#include "Dispatcher.h"

#include "eckit/io/Buffer.h"

#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/Message.h"
#include "multio/server/msg_tag.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Dispatcher::Dispatcher(const Transport& trans) :
    transport_(trans),
    planFactory(transport_.no_clients()) {}

void Dispatcher::registerPlan(const Message& msg) {
    ASSERT(msg.tag() == msg_tag::plan_data);
    auto plan_name = fetch_metadata(msg).get<std::string>("name");
    if (registered_plans_.find(plan_name) != end(registered_plans_)) {
        // Plan exists already -- nothing to do
        return; // Or, better, throw an error?
    }

    // You'd want std::optional here
    if (not planFactory.try_create(msg)) {
        return; // Plan not yet complete -- nothing more to do
    }

    registered_plans_.emplace(plan_name, planFactory.handOver(plan_name));
}

void Dispatcher::feedPlan(const Message& msg) {
    auto plan_name = fetch_metadata(msg).get<std::string>("field_type");
    if (registered_plans_.find(plan_name) == end(registered_plans_)) {
        backlog_[plan_name].push_back(std::move(msg));
    } else {
        processBacklog(plan_name);
        registered_plans_.at(plan_name).process(msg);
    }
}

void Dispatcher::listen() {
    eckit::Log::info() << "Rank: " << transport_.global_rank() << ", Started listening..."
                       << std::endl;
    auto counter = 0u;
    do {
        Message msg(0);
        transport_.receiveFromClient(msg);
        switch (msg.tag()) {
            case msg_tag::plan_data:
                registerPlan(msg);
                break;
            case msg_tag::field_data:
                feedPlan(std::move(msg));
                break;
            case msg_tag::forecast_complete:
                ++counter;
                break;
            default:
                ASSERT(false);
        }
    } while (not allPartsArrived(counter));
    eckit::Log::info() << "Rank: " << transport_.global_rank() << ", Done with listening..."
                       << std::endl;
}

// Private member functions

void Dispatcher::processBacklog(const std::string& plan_name) {
    if (backlog_.find(plan_name) != end(backlog_)) {
        for (auto&& m : backlog_.at(plan_name)) {
            registered_plans_.at(plan_name).process(std::move(m));
        }
        backlog_.erase(plan_name);
    }
}

bool Dispatcher::allPartsArrived(const unsigned counter) const {
    return (counter == transport_.no_clients());
}

void Dispatcher::print(std::ostream& os) const {
    os << "Dispatcher initialised with " << transport_;
}


}  // namespace server
}  // namespace multio
