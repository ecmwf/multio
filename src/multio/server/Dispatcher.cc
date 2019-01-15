
#include "Dispatcher.h"

#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"
#include "eckit/exception/Exceptions.h"

#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/Message.h"
#include "multio/server/msg_tag.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Dispatcher::Dispatcher(const Transport& trans) : transport_(trans) {}

std::string Dispatcher::registerPlan(const Message& msg) {
    ASSERT(msg.tag() == msg_tag::plan_data);
    auto plan_name = fetch_metadata(msg).get<std::string>("plan_name");
    if (registeredPlans_.find(plan_name) != end(registeredPlans_)) {
        ASSERT(false);
    }

    // You'd want std::optional here
    if (not planAssembler_.tryCreate(msg)) {
        return std::string{}; // Plan not yet complete -- nothing more to do
    }

    registeredPlans_.emplace(plan_name, planAssembler_.handOver(plan_name));
    return plan_name;
}

void Dispatcher::feedPlan(const Message& msg) {
    auto plan_name = fetch_metadata(msg).get<std::string>("plan_name");
    registeredPlans_.at(plan_name).process(msg);
}

void Dispatcher::listen() {
    eckit::Log::info() << "Rank: " << transport_.globalRank() << ", Started listening..."
                       << std::endl;
    auto counter = 0u;
    do {
        Message msg(0);
        transport_.receive(msg);
        switch (msg.tag()) {
            case msg_tag::plan_data: {
                auto plan_name = registerPlan(msg);
                if (not plan_name.empty()) {
                    Message ack(0, -1, msg_tag::plan_complete);
                    ack.write(plan_name.c_str(), plan_name.size());
                    transport_.notifyAllClients(ack);
                }
            } break;
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
    eckit::Log::info() << "Rank: " << transport_.globalRank() << ", Done with listening..."
                       << std::endl;
}

// Private member functions

bool Dispatcher::allPartsArrived(const unsigned counter) const {
    return (counter == transport_.noClients());
}

void Dispatcher::print(std::ostream& os) const {
    os << "Dispatcher initialised with " << transport_;
}


}  // namespace server
}  // namespace multio
