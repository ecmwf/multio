/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Listener.h"

#include <unistd.h>

#include <fstream>
#include <functional>
#include <typeinfo>

#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/ResourceUsage.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"

#include "multio/server/Dispatcher.h"
#include "multio/transport/Transport.h"
#include "multio/transport/TransportRegistry.h"
#include "multio/util/ScopedThread.h"

namespace multio::server {

using message::Message;
using transport::Transport;
using util::ScopedThread;

Listener::Listener(const config::ComponentConfiguration& compConf, Transport& trans) :
    FailureAware(compConf),
    dispatcher_{std::make_unique<Dispatcher>(compConf, msgQueue_)},
    transport_{trans},
    clientCount_{transport_.clientPeers().size()},
    msgQueue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024 * 1024)) {}

Listener::~Listener() = default;

util::FailureHandlerResponse Listener::handleFailure(util::OnReceiveError t, const util::FailureContext& c,
                                                     util::DefaultFailureState&) const {
    msgQueue_.interrupt(c.eptr);
    transport::TransportRegistry::instance().abortAll(c.eptr);

    return util::FailureHandlerResponse::Rethrow;
};

void Listener::start() {

    eckit::ResourceUsage usage{"multio listener"};

    // Store thread errors
    ScopedThread lstnThread{std::thread{[&]() { this->listen(); }}};

    ScopedThread dpatchThread{std::thread{[&]() { dispatcher_->dispatch(); }}};

    withFailureHandling([&]() {
        do {
            Message msg = transport_.receive();

            switch (msg.tag()) {
                case Message::Tag::Open:
                    connections_.insert(msg.source());
                    ++openedCount_;
                    LOG_DEBUG_LIB(LibMultio)
                        << "*** OPENING connection to " << msg.source() << ":    client count = " << clientCount_
                        << ", opened count = " << openedCount_ << ", active connections = " << connections_.size()
                        << std::endl;
                    break;

                case Message::Tag::Close:
                    connections_.erase(connections_.find(msg.source()));
                    LOG_DEBUG_LIB(LibMultio)
                        << "*** CLOSING connection to " << msg.source() << ":    client count = " << clientCount_
                        << ", opened count = " << openedCount_ << ", active connections = " << connections_.size()
                        << std::endl;
                    break;

                case Message::Tag::Domain:
                case Message::Tag::Parametrization:
                case Message::Tag::Mask:
                case Message::Tag::Notification:
                case Message::Tag::Flush:
                case Message::Tag::Field:
                    checkConnection(msg.source());
                    LOG_DEBUG_LIB(LibMultio) << "*** Message received: " << msg << std::endl;
                    msgQueue_.emplace(std::move(msg));
                    break;

                default:
                    std::ostringstream oss;
                    oss << "Unhandled message: " << msg << std::endl;
                    throw eckit::SeriousBug(oss.str());
            }
        } while (moreConnections() && msgQueue_.checkInterrupt());
    });

    LOG_DEBUG_LIB(LibMultio) << "*** STOPPED listening loop " << std::endl;

    msgQueue_.close();

    LOG_DEBUG_LIB(LibMultio) << "*** CLOSED message queue " << std::endl;
}

void Listener::listen() {
    withFailureHandling([this]() {
        do {
            transport_.listen();
        } while (msgQueue_.checkInterrupt() && !msgQueue_.closed());
    });
}

bool Listener::moreConnections() const {
    return !connections_.empty() || openedCount_ != clientCount_;
}

void Listener::checkConnection(const message::Peer& conn) const {
    if (connections_.find(conn) == end(connections_)) {
        std::ostringstream oss;
        oss << "Connection to " << conn << " is not open";
        throw eckit::SeriousBug{oss.str(), Here()};
    }
}

}  // namespace multio::server
