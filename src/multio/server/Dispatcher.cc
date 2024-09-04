#include "Dispatcher.h"

#include <fstream>

#include "eckit/config/LocalConfiguration.h"

#include "multio/LibMultio.h"
#include "multio/action/Plan.h"

#include "multio/domain/Mappings.h"
#include "multio/domain/Mask.h"

#include "multio/util/ScopedTimer.h"

using eckit::LocalConfiguration;

namespace multio::server {

Dispatcher::Dispatcher(const config::ComponentConfiguration& compConf, eckit::Queue<message::Message>& queue) :
    FailureAware(compConf), queue_{queue} {
    timer_.start();

    eckit::Log::debug<LibMultio>() << compConf.parsedConfig() << std::endl;

    config::ComponentConfiguration::SubComponentConfigurations plans = compConf.subComponents("plans");
    for (auto&& subComp : plans) {
        eckit::Log::debug<LibMultio>() << subComp.parsedConfig() << std::endl;
        plans_.emplace_back(std::make_unique<action::Plan>(std::move(subComp)));
    }
}

util::FailureHandlerResponse Dispatcher::handleFailure(util::OnDispatchError t, const util::FailureContext& c,
                                                       util::DefaultFailureState&) const {
    queue_.interrupt(c.eptr);
    return util::FailureHandlerResponse::Rethrow;
};

Dispatcher::~Dispatcher() = default;

void Dispatcher::dispatch() {
    util::ScopedTimer timer{timing_};
    withFailureHandling([&]() {
        try {
            message::Message msg;
            auto sz = queue_.pop(msg);
            while (sz >= 0) {
                handle(msg);
                LOG_DEBUG_LIB(multio::LibMultio) << "Size of the dispatch queue: " << sz << std::endl;
                sz = queue_.pop(msg);
            }
        }
        catch (const multio::util::FailureAwareException& ex) {
            std::cerr << ex << std::endl;
            throw;
        }
    });
}

void Dispatcher::handle(const message::Message& msg) const {
    switch (msg.tag()) {
        case message::Message::Tag::Domain:
            domain::Mappings::instance().add(msg);
            break;

        case message::Message::Tag::Mask:
            domain::Mask::instance().add(msg);
            break;

        default:
            for (const auto& plan : plans_) {
                plan->process(msg);
            }
    }
}

}  // namespace multio::server
