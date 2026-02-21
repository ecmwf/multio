#include "MultIOTransportProgress.h"

#include "multio/transport/Transport.h"
#include "multio/transport/TransportRegistry.h"
#include "multio/util/FailureHandling.h"


namespace multio::server {

using transport::TransportFactory;

MultIOTransportProgress::MultIOTransportProgress(
        const config::ComponentConfiguration& compConf,
        MultIOQueue& queue,
        MultIOProfilerState& profiler) :

    transport_(TransportFactory::instance().build(
        compConf.parsedConfig().getString("transport"),
        compConf)),

    queue_(queue),
    profiler_(profiler) {}


void MultIOTransportProgress::run()
{
    do {
        transport_->listen();
        profiler_.transport().listenIterations.fetch_add(
            1, std::memory_order_relaxed);
    } while (queue_.checkInterrupt() && !queue_.closed());

}


}