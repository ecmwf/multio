
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
#include "multio/server/ScopedThread.h"

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
    // Start listening thread
    ScopedThread scThread{std::thread{[this]() { this->listen(); }}};

    // Other thread does the dispatching
    do {
        dispatchNext();
    } while (not (allPartsArrived() && msgQueue_.empty()) );
}

// Private member functions

void Dispatcher::listen() {
    eckit::Log::info() << "Rank: " << transport_.globalRank() << ", Started listening..."
                       << std::endl;

    do {
        Message msg(0);
        transport_.receive(msg);

        if (msg.tag() == msg_tag::forecast_complete) {
            ++counter_;
        }
        else {
            msgQueue_.push(std::make_shared<Message>(std::move(msg)));
        }

    } while (not allPartsArrived());

    eckit::Log::info() << "Rank: " << transport_.globalRank() << ", Done with listening..."
                       << std::endl;
}

void Dispatcher::dispatchNext() {
    auto msg = msgQueue_.pop();
    switch (msg->tag()) {
        case msg_tag::message_data:
            Mappings::instance().add(*msg);
            break;

        case msg_tag::field_data:
            feedPlans(msg);
            break;

        case msg_tag::step_complete:
            feedPlans(msg);
            break;

        default:
            ASSERT(false);
    }
}

bool Dispatcher::allPartsArrived() {
    return (counter_ == transport_.noClients());
}

void Dispatcher::print(std::ostream& os) const {
    os << "Dispatcher initialised with " << transport_;
}


}  // namespace server
}  // namespace multio
