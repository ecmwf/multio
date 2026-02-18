#include "MultIOTransportProgress.h"

#include "multio/transport/TransportFactory.h"
#include "multio/util/FailureHandling.h"

namespace multio::server {

using transport::TransportFactory;

MultIOTransportProgress::MultIOTransportProgress(
        const config::ComponentConfiguration& compConf,
        MultIOQueue& queue,
        impl::MultIOProfilerTransportState& profiler) :

    transport_(TransportFactory::instance().build(
        compConf.parsedConfig().getString("transport"),
        compConf)),

    queue_(queue),
    profiler_(profiler) {}


void MultIOTransportProgress::run()
{
    util::withFailureHandling([&]() {

        do {
            transport_->listen();

            profiler_.listenIterations.fetch_add(
                1, std::memory_order_relaxed);
        } while (queue_.impl().checkInterrupt() && !queue_.closed())

    });
}


}