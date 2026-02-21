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
#include "MultIOProfiler.h"

namespace multio::server {

struct ComposableServerFailureTraits {
    using OnErrorType = util::OnServerError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence = util::integer_sequence<OnErrorType, OnErrorType::Propagate, OnErrorType::Recover,
                                               OnErrorType::AbortTransport>;
    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnErrorType::Propagate; };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline std::string componentName() { return std::string("ComposableServer"); };
};

class MultIOComposableServer :
    public config::MultioConfigurationHolder,
    public util::FailureAware<ComposableServerFailureTraits> {

public:

    explicit MultIOComposableServer(config::MultioConfiguration&&);

    ~MultIOComposableServer();

    void run();

    util::FailureHandlerResponse handleFailure(
        util::OnServerError,
        const util::FailureContext&,
        util::DefaultFailureState&) const override;

private:

    // CONFIG
    eckit::LocalConfiguration serverConf_;
    config::ComponentConfiguration compConf_;

    bool hasProfiler_{false};
    std::size_t dispatcherCount_{1};

    // DATA
    mutable MultIOProfilerState profilerState_;
    mutable MultIOQueue queue_;

    // RUNNERS
    mutable MultIOTransportProgress transportProgress_;
    mutable MultIOReceiver receiver_;
    mutable std::vector<std::unique_ptr<MultIODispatcherRunner>> dispatchers_;
    mutable MultIOProfiler profilerRunner_;

    // THREADS
    std::vector<std::thread> threads_;
};

}  // namespace multio::server