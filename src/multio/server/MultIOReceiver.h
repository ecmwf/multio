#pragma once

#include <set>

#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/message/Peer.h"
#include "multio/transport/Transport.h"
#include "multio/util/FailureHandling.h"

#include "MultIOQueue.h"
#include "MultIOProfilerReceiverState.h"

namespace multio::server {

struct ReceiverFailureTraits {
    using OnErrorType = util::OnReceiveError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence =
        util::integer_sequence<OnErrorType, OnErrorType::Propagate>;

    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }

    static inline OnErrorType defaultOnErrorTag() {
        return OnErrorType::Propagate;
    }

    static inline std::string configKey() {
        return "on-receive-error";
    }

    static inline FailureOptions parseFailureOptions(
        const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    }

    static inline std::string componentName() { return "Receiver"; }
};

class MultIOReceiver :
    public util::FailureAware<ReceiverFailureTraits> {
public:

    MultIOReceiver(const config::ComponentConfiguration& compConf,
                   transport::Transport& transport,
                   MultIOQueue& queue,
                   MultIOProfilerReceiverState& profiler);

    void run();

    util::FailureHandlerResponse handleFailure(
        util::OnReceiveError,
        const util::FailureContext&,
        util::DefaultFailureState&) const override;

private:
    bool moreConnections() const;
    void checkConnection(const message::Peer&) const;

private:
    transport::Transport& transport_;
    MultIOQueue& queue_;
    MultIOProfilerReceiverState& profiler_;

    size_t openedCount_{0};
    size_t clientCount_{0};
    size_t syncCount_{0};

    std::set<message::Peer> connections_;
};

}  // namespace multio::server