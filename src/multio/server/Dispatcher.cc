
#include "Dispatcher.h"

#include <thread>

#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"
#include "eckit/exception/Exceptions.h"

#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/Mappings.h"
#include "multio/server/Message.h"
#include "multio/server/msg_tag.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Dispatcher::Dispatcher(const Transport& trans) :
    transport_(trans),
    registeredPlans_{PlanAssembler{}.createAllPlans()} {}

void Dispatcher::feedPlans(std::shared_ptr<Message> msg) {
    for (const auto& plan : registeredPlans_) {
        plan.process(msg);
    }
}

void Dispatcher::eventLoop() {
    std::thread t([this] { this->listen(); });

    // listen();

    eckit::Log::info() << "Rank: " << transport_.globalRank() << ", Started listening..."
                       << std::endl;

    auto counter = 0u;
    do {
        std::cout << "About to pop message... " << std::endl;
        std::cout << "    Size of message queue: " << msgQueue_.size() << std::endl;
        auto msg = msgQueue_.pop();
        std::cout << "Popped message... " << std::endl;
        switch (msg->tag()) {
            case msg_tag::message_data: {
                Mappings::instance().add(*msg);
            } break;

            case msg_tag::field_data:
                feedPlans(msg);
                break;

            case msg_tag::step_complete:
                feedPlans(msg);
                break;

            case msg_tag::forecast_complete:
                ++counter;
                break;

            default:
                ASSERT(false);
        }
    } while (not (allPartsArrived_ && msgQueue_.empty()));
    eckit::Log::info() << "Rank: " << transport_.globalRank() << ", Done with listening..."
                       << std::endl;
    t.join();
    std::cout << "Thread joined..." << std::endl;
}

// Private member functions

void Dispatcher::listen() {
    auto counter = 0u;
    do {
        Message msg(0);
        transport_.receive(msg);

        if (msg.tag() == msg_tag::forecast_complete) {
            ++counter;
        } else {
            msgQueue_.push(std::make_shared<Message>(std::move(msg)));
            std::cout << "Just pushed a message... " << std::endl;
            std::cout << "    Size of message queue: " << msgQueue_.size() << std::endl;
        }

    } while (not allPartsArrived(counter));
}

bool Dispatcher::allPartsArrived(const unsigned counter) {
    allPartsArrived_ = (counter == transport_.noClients());
    return allPartsArrived_;
}

void Dispatcher::print(std::ostream& os) const {
    os << "Dispatcher initialised with " << transport_;
}


}  // namespace server
}  // namespace multio
