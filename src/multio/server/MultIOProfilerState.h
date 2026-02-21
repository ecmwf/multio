#pragma once

#include <atomic>
#include <cstdint>
#include <vector>

namespace multio::server {

namespace impl {
struct alignas(64) MultIOProfilerQueueState {

    std::atomic<uint64_t> pushes{0};
    std::atomic<uint64_t> pops{0};

    std::atomic<uint64_t> bytesIn{0};
    std::atomic<uint64_t> bytesOut{0};

    std::atomic<uint64_t> fill{0};
    std::atomic<uint64_t> maxFill{0};

};

struct alignas(64) MultIOProfilerTransportState {

    std::atomic<uint64_t> listenIterations{0};

    std::atomic<uint64_t> recvCount{0};
    std::atomic<uint64_t> recvBytes{0};

    std::atomic<uint64_t> probeCount{0};
};


struct alignas(64) MultIOProfilerReceiverState {

    std::atomic<uint64_t> messagesEnqueued{0};

    std::atomic<uint64_t> syncMessages{0};
    std::atomic<uint64_t> openConnections{0};
    std::atomic<uint64_t> closeConnections{0};

    std::atomic<uint64_t> receiveLoops{0};
};

struct alignas(64) MultIOProfilerDispatcherState {

    std::atomic<uint64_t> messagesDispatched{0};
    std::atomic<uint64_t> dispatchFailures{0};

    std::atomic<uint64_t> emptyQueuePops{0};

};

}  // namespace impl

class MultIOProfilerState {
public:

    explicit MultIOProfilerState(std::size_t numDispatchers) :
        dispatcherStates_(numDispatchers) {}

    impl::MultIOProfilerQueueState& queue() noexcept { return queueState_; }
    impl::MultIOProfilerTransportState& transport() noexcept { return transportState_; }
    impl::MultIOProfilerReceiverState& receiver() noexcept { return receiverState_; }

    impl::MultIOProfilerDispatcherState& dispatcher(std::size_t i) noexcept {
        return dispatcherStates_[i];
    }


    std::vector<impl::MultIOProfilerDispatcherState>& dispatchers() noexcept {
        return dispatcherStates_;
    }



    const impl::MultIOProfilerQueueState& queue() const noexcept { return queueState_; }
    const impl::MultIOProfilerTransportState& transport() const noexcept { return transportState_; }
    const impl::MultIOProfilerReceiverState& receiver() const noexcept { return receiverState_; }

    const impl::MultIOProfilerDispatcherState& dispatcher(std::size_t i) const noexcept {
        return dispatcherStates_[i];
    }

    const std::vector<impl::MultIOProfilerDispatcherState>& dispatchers() const noexcept {
        return dispatcherStates_;
    }


private:

    impl::MultIOProfilerQueueState queueState_;
    impl::MultIOProfilerTransportState transportState_;
    impl::MultIOProfilerReceiverState receiverState_;

    std::vector<impl::MultIOProfilerDispatcherState> dispatcherStates_;
};


}  // namespace multio::server