#include "MultIODispatcherRunner.h"

#include "multio/util/FailureHandling.h"
#include "multio/LibMultio.h"

namespace multio::server {

using message::Message;

MultIODispatcherRunner::MultIODispatcherRunner(
        const config::ComponentConfiguration& compConf,
        MultIOQueue& queue,
        transport::Transport& transport,
        impl::MultIOProfilerDispatcherState& profiler) :

    dispatcher_(std::make_unique<Dispatcher>(compConf,
                                             queue.impl(),
                                             transport)),
    queue_(queue),
    profiler_(profiler) {}


void MultIODispatcherRunner::run() {

    util::withFailureHandling([&]() {

        Message msg;

        long sz = queue_.pop(msg);

        while (sz >= 0) {

            profiler_.messagesDispatched.fetch_add(
                1, std::memory_order_relaxed);

            try {
                dispatcher_->handle(std::move(msg));
            }
            catch (...) {
                profiler_.dispatchFailures.fetch_add(
                    1, std::memory_order_relaxed);
                throw;
            }

            sz = queue_.pop(msg);

            if (sz < 0 && !queue_.closed()) {
                profiler_.emptyQueuePops.fetch_add(
                    1, std::memory_order_relaxed);
            }
        }

    });

    LOG_DEBUG_LIB(multio::LibMultio)
        << "*** Dispatcher loop stopped" << std::endl;
}

}