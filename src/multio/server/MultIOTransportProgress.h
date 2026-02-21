#pragma once

#include <memory>

#include "multio/transport/Transport.h"
#include "multio/config/ComponentConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "MultIOQueue.h"
#include "MultIOProfilerState.h"

namespace multio::server {

class MultIOTransportProgress {
public:

    MultIOTransportProgress(const config::ComponentConfiguration& compConf,
                            MultIOQueue& queue,
                            MultIOProfilerState& profiler);

    transport::Transport& transport() noexcept { return *transport_; }


    void run();

private:
    MultIOQueue& queue_;
    std::unique_ptr<transport::Transport> transport_;
    MultIOProfilerState& profiler_;
};

}
