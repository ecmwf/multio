#pragma once

#include "multio/server/Dispatcher.h"

#include "MultIOQueue.h"
#include "MultIOProfilerDispatcherState.h"

namespace multio::server {

class MultIODispatcherRunner {
public:

    MultIODispatcherRunner( MultIOQueue& queue,
                            MultIOProfilerDispatcherState& profiler) noexcept;

    void run();

private:
    Dispatcher dispatcher_;
    MultIOQueue& queue_;
    MultIOProfilerDispatcherState& profiler_;
};

}  // namespace multio::server