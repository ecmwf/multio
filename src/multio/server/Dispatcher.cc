#include "Dispatcher.h"

#include <fstream>

#include "eckit/config/LocalConfiguration.h"

#include "multio/LibMultio.h"
#include "multio/action/Plan.h"

#include "multio/domain/Mappings.h"
#include "multio/domain/Mask.h"

#include "multio/util/ScopedTimer.h"
#include "multio/util/logfile_name.h"

using eckit::LocalConfiguration;

namespace multio {
namespace server {

Dispatcher::Dispatcher(const util::ConfigurationContext& confCtx, std::shared_ptr<std::atomic<bool>> cont): FailureAware(confCtx), continue_{std::move(cont)} {
    timer_.start();

    eckit::Log::debug<LibMultio>() << confCtx.config() << std::endl;

    util::ConfigurationContext::SubConfigurationContexts plans = confCtx.subContexts("plans", util::ComponentTag::Plan);
    for (auto&& subCtx: plans) {
        eckit::Log::debug<LibMultio>() << subCtx.config() << std::endl;
        plans_.emplace_back(new action::Plan(std::move(subCtx)));
    }
}

util::FailureHandlerResponse Dispatcher::handleFailure(util::OnDispatchError t, const util::FailureContext& c, util::DefaultFailureState&) const {
    continue_->store(false, std::memory_order_relaxed);
    return util::FailureHandlerResponse::Rethrow;
};

Dispatcher::~Dispatcher() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};
    logFile << "\n ** Total wall-clock time spent in dispatcher " << eckit::Timing{timer_}.elapsed_
            << "s -- of which time spent with dispatching " << timing_ << "s" << std::endl;
}

void Dispatcher::dispatch(eckit::Queue<message::Message>& queue) {
    util::ScopedTimer timer{timing_};
    withFailureHandling([&]() {
        message::Message msg;
        auto sz = queue.pop(msg);
        while (sz >= 0 && continue_->load(std::memory_order_consume)) {
            handle(msg);
            LOG_DEBUG_LIB(multio::LibMultio) << "Size of the dispatch queue: " << sz << std::endl;
            sz = queue.pop(msg);
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

}  // namespace server
}  // namespace multio
