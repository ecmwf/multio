#include "Dispatcher.h"

#include <fstream>

#include "eckit/config/LocalConfiguration.h"

#include "multio/LibMultio.h"
#include "multio/action/Plan.h"
#include "multio/message/Parametrization.h"

#include "multio/domain/Mappings.h"
#include "multio/domain/Mask.h"

using eckit::LocalConfiguration;

namespace multio::server {

using config::ComponentConfiguration;

Dispatcher::Dispatcher(const config::ComponentConfiguration& compConf, eckit::Queue<message::Message>& queue) :
    FailureAware(compConf), queue_{queue} {

    eckit::Log::debug<LibMultio>() << compConf.parsedConfig() << std::endl;

    config::ComponentConfiguration::SubComponentConfigurations plans = compConf.subComponents("plans");

    plans_ = action::Plan::makePlans(compConf.parsedConfig().getSubConfigurations("plans"), compConf.multioConfig());
}

util::FailureHandlerResponse Dispatcher::handleFailure(util::OnDispatchError t, const util::FailureContext& c,
                                                       util::DefaultFailureState&) const {
    queue_.interrupt(c.eptr);
    return util::FailureHandlerResponse::Rethrow;
};

Dispatcher::~Dispatcher() = default;

void Dispatcher::dispatch() {
    util::ScopedTiming<> timer{timing_};
    withFailureHandling([&]() {
        try {
            message::Message msg;
            auto sz = queue_.pop(msg);
            while (sz >= 0) {
                handle(std::move(msg));
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

void Dispatcher::handle(message::Message msg) const {
    switch (msg.tag()) {
        case message::Message::Tag::Domain:
            domain::Mappings::instance().add(std::move(msg));
            break;

        case message::Message::Tag::Mask:
            domain::Mask::instance().add(std::move(msg));
            break;

        case message::Message::Tag::Parametrization:
            LOG_DEBUG_LIB(multio::LibMultio) << "Server received parametrization: " << msg << std::endl;
            message::Parametrization::instance().update(std::move(msg));
            break;

        default:
            // TODO add proper PlanExecuter that checks select paths befare...
            for (const auto& plan : plans_) {
                plan->process(msg);
            }
    }
}

}  // namespace multio::server
