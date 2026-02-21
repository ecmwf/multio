#pragma once


#include "MultIOQueue.h"
#include "MultIOProfilerState.h"
#include "multio/action/Plan.h"

#include "multio/transport/Transport.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/util/FailureHandling.h"
namespace multio::server {

struct DispatcherRunnerFailureTraits {
    using OnErrorType = util::OnDispatchError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence = util::integer_sequence<OnErrorType, OnErrorType::Propagate>;
    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnErrorType::Propagate; };
    static inline std::string configKey() { return std::string("on-dispatch-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    };
    static inline std::string componentName() { return std::string("Dispatcher"); };
};

class MultIODispatcherRunner : public util::FailureAware<DispatcherRunnerFailureTraits> {
public:

    MultIODispatcherRunner( const config::ComponentConfiguration& compConf,
                            multio::transport::Transport& transport,
                            MultIOQueue& queue,
                            MultIOProfilerState& profiler,
                            std::size_t tid ) noexcept;
    void run();

    void handle(message::Message msg) const;

    util::FailureHandlerResponse handleFailure(
        util::OnDispatchError,
        const util::FailureContext&,
        util::DefaultFailureState&) const override;

private:
    MultIOQueue& queue_;
    MultIOProfilerState& profiler_;
    multio::transport::Transport& transport_;
    const std::size_t tid_;
    std::vector<std::unique_ptr<action::Plan>> plans_;
};

}  // namespace multio::server