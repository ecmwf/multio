#pragma once

#include <memory>
#include <thread>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "multio/config/MultioConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/util/FailureHandling.h"

#include "multio/transport/Transport.h"

#include "MultIOQueue.h"
#include "MultIOProfilerState.h"
#include "MultIOTransportProgress.h"
#include "MultIOReceiver.h"
#include "MultIODispatcherRunner.h"
#include "MultIOQueueProfiler.h"

namespace multio::server {

class MultIOComposableServer :
    public config::MultioConfigurationHolder,
    public util::FailureAware<ServerFailureTraits> {

public:

    explicit MultIOComposableServer(config::MultioConfiguration&&);

    ~MultIOComposableServer();

    void run();

    util::FailureHandlerResponse handleFailure(
        util::OnServerError,
        const util::FailureContext&,
        util::DefaultFailureState&) const override;

private:

    static eckit::LocalConfiguration getServerConf(
        const config::MultioConfiguration&);

private:

    // CONFIG
    eckit::LocalConfiguration serverConf_;
    config::ComponentConfiguration compConf_;

    // DATA
    MultIOProfilerState profilerState_;
    MultIOQueue queue_;

    // RUNNERS
    MultIOTransportProgress transportProgress_;
    MultIOReceiver receiver_;
    std::vector<std::unique_ptr<MultIODispatcherRunner>> dispatchers_;
    std::unique_ptr<MultIOQueueProfiler> profilerRunner_;

    // THREADS
    std::vector<std::thread> threads_;
};

}  // namespace multio::server