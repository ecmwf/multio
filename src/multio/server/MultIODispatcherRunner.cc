#include "MultIODispatcherRunner.h"

#include "multio/util/FailureHandling.h"
#include "multio/LibMultio.h"
#include "multio/action/Plan.h"
#include "multio/message/Parametrization.h"
#include "multio/domain/Mappings.h"
#include "multio/domain/Mask.h"

namespace multio::server {

namespace impl {

std::vector<std::unique_ptr<action::Plan>> makePlans( const config::ComponentConfiguration& compConf, std::size_t tid) {

    config::ComponentConfiguration::SubComponentConfigurations plans = compConf.subComponents("plans");

    return action::Plan::makePlans(compConf.parsedConfig().getSubConfigurations("plans"), compConf.multioConfig());

}
}


using message::Message;

MultIODispatcherRunner::MultIODispatcherRunner(
        const config::ComponentConfiguration& compConf,
        multio::transport::Transport& transport,
        MultIOQueue& queue,
        MultIOProfilerState& profiler,
        std::size_t tid) noexcept:
    FailureAware(compConf),
    queue_(queue),
    profiler_(profiler),
    transport_(transport),
    tid_(tid),
    plans_(impl::makePlans(compConf, tid)) {}



    //dispatcher_(std::make_unique<Dispatcher>(compConf,
    //                                         queue.impl(),
    //                                         transport)),

void MultIODispatcherRunner::run() {
    withFailureHandling([&]() {

        Message msg;

        long sz = queue_.pop(msg);

        while (sz >= 0) {

            profiler_.dispatcher(tid_).messagesDispatched.fetch_add(
                1, std::memory_order_relaxed);

            try {
                handle(std::move(msg));
            }
            catch (...) {
                profiler_.dispatcher(tid_).dispatchFailures.fetch_add(
                    1, std::memory_order_relaxed);
                throw;
            }

            sz = queue_.pop(msg);

            if (sz < 0 && !queue_.closed()) {
                profiler_.dispatcher(tid_).emptyQueuePops.fetch_add(
                    1, std::memory_order_relaxed);
            }
        }

    });

    LOG_DEBUG_LIB(multio::LibMultio)
        << "*** Dispatcher loop stopped" << std::endl;
}

util::FailureHandlerResponse
MultIODispatcherRunner::handleFailure(util::OnDispatchError,
                                      const util::FailureContext& ctx,
                                      util::DefaultFailureState&) const {
    queue_.interrupt(ctx.eptr);
    return util::FailureHandlerResponse::Rethrow;
}


void MultIODispatcherRunner::handle(message::Message msg) const {
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

        case message::Message::Tag::Synchronization:
            transport_.synchronize();
            break;

        default:
            // TODO add proper PlanExecuter that checks select paths befare...
            for (const auto& plan : plans_) {
                plan->process(msg);
            }
    }
}


}