#include "MultIOReceiver.h"

#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/transport/TransportRegistry.h"

namespace multio::server {

using message::Message;

MultIOReceiver::MultIOReceiver(
    const config::ComponentConfiguration& compConf,
    transport::Transport& transport,
    MultIOQueue& queue,
    MultIOProfilerReceiverState& profiler) :
    FailureAware(compConf),
    transport_(transport),
    queue_(queue),
    profiler_(profiler),
    clientCount_(transport_.clientPeers().size()) {}


util::FailureHandlerResponse
MultIOReceiver::handleFailure(util::OnReceiveError,
                              const util::FailureContext& ctx,
                              util::DefaultFailureState&) const {

    queue_.impl().interrupt(ctx.eptr);
    transport::TransportRegistry::instance().abortAll(ctx.eptr);

    return util::FailureHandlerResponse::Rethrow;
}


void MultIOReceiver::run() {

    util::withFailureHandling([&]() {

        do {

            profiler_.receiveLoops.fetch_add(
                1, std::memory_order_relaxed);

            Message msg = transport_.receive();

            switch (msg.tag()) {

                case Message::Tag::Open:
                    connections_.insert(msg.source());
                    ++openedCount_;

                    profiler_.openConnections.fetch_add(
                        1, std::memory_order_relaxed);

                    break;

                case Message::Tag::Close:
                    connections_.erase(msg.source());

                    profiler_.closeConnections.fetch_add(
                        1, std::memory_order_relaxed);

                    break;

                case Message::Tag::Synchronization:

                    checkConnection(msg.source());

                    profiler_.syncMessages.fetch_add(
                        1, std::memory_order_relaxed);

                    if (++syncCount_ == clientCount_) {
                        queue_.emplace(std::move(msg));
                        profiler_.messagesEnqueued.fetch_add(
                            1, std::memory_order_relaxed);
                        syncCount_ = 0;
                    }
                    break;

                case Message::Tag::Domain:
                case Message::Tag::Mask:
                case Message::Tag::Parametrization:
                case Message::Tag::Notification:
                case Message::Tag::Flush:
                case Message::Tag::Field:

                    checkConnection(msg.source());

                    queue_.emplace(std::move(msg));

                    profiler_.messagesEnqueued.fetch_add(
                        1, std::memory_order_relaxed);

                    break;

                default: {
                    std::ostringstream oss;
                    oss << "Unhandled message: " << msg;
                    throw eckit::SeriousBug(oss.str());
                }
            }

        } while (moreConnections() &&
                 queue_.impl().checkInterrupt());

    });

    LOG_DEBUG_LIB(multio::LibMultio)
        << "*** Receiver loop stopped" << std::endl;

    queue_.close();
}


bool MultIOReceiver::moreConnections() const {
    return !connections_.empty() || openedCount_ != clientCount_;
}


void MultIOReceiver::checkConnection(
    const message::Peer& conn) const {

    if (connections_.find(conn) == connections_.end()) {
        std::ostringstream oss;
        oss << "Connection to " << conn << " is not open";
        throw eckit::SeriousBug{oss.str(), Here()};
    }
}

}  // namespace multio::server