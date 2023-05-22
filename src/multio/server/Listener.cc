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

#include "multio/util/ScopedThread.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/server/Dispatcher.h"
#include "multio/transport/Transport.h"

namespace multio {
namespace server {

using message::Message;
using util::ScopedThread;
using transport::Transport;

Listener::Listener(const util::ConfigurationContext& confCtx, Transport& trans):
        FailureAware(confCtx),
    continue_{std::make_shared<std::atomic<bool>>(true)},
    dispatcher_{std::make_unique<Dispatcher>(confCtx.recast(util::ComponentTag::Dispatcher), continue_)},
    transport_{trans},
    clientCount_{transport_.clientPeers().size()},
    msgQueue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE",1024*1024)) {
}

Listener::~Listener() = default;

util::FailureHandlerResponse Listener::handleFailure(util::OnReceiveError t, const util::FailureContext& c, util::DefaultFailureState&) const {
    msgQueue_.close();  // TODO: msgQueue_ pop is blocking in dispatch.... redesign to have better awareness on blocking
                        // positions to safely stop and restart
    continue_->store(false, std::memory_order_release);
    return util::FailureHandlerResponse::Rethrow;
};

void Listener::start() {

    eckit::ResourceUsage usage{"multio listener"};

    // Store thread errors
    std::exception_ptr lstnExcPtr;
    std::exception_ptr dpatchExcPtr;

    ScopedThread lstnThread{std::thread{[&](){ try{ this->listen(); } catch (...) { lstnExcPtr = std::current_exception(); } }}};

    ScopedThread dpatchThread{std::thread{[&](){ try { dispatcher_->dispatch(msgQueue_); } catch (...) { dpatchExcPtr = std::current_exception(); } }}};

    withFailureHandling([&]() {
        do {
            Message msg = transport_.receive();

            switch (msg.tag()) {
                case Message::Tag::Open:
                    connections_.insert(msg.source());
                    ++openedCount_;
                    LOG_DEBUG_LIB(LibMultio)
                        << "*** OPENING connection to " << msg.source()
                        << ":    client count = " << clientCount_ << ", opened count = " << openedCount_
                        << ", active connections = " << connections_.size() << std::endl;
                    break;

                case Message::Tag::Close:
                    connections_.erase(connections_.find(msg.source()));
                    LOG_DEBUG_LIB(LibMultio)
                        << "*** CLOSING connection to " << msg.source()
                        << ":    client count = " << clientCount_ << ", opened count = " << openedCount_
                        << ", active connections = " << connections_.size() << std::endl;
                    break;

                case Message::Tag::Domain:
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
        } while (moreConnections() && continue_->load(std::memory_order_consume));
    });

    LOG_DEBUG_LIB(LibMultio) << "*** STOPPED listening loop " << std::endl;

    msgQueue_.close();

    LOG_DEBUG_LIB(LibMultio) << "*** CLOSED message queue " << std::endl;

    // Propagate possible thread errors
    if (lstnExcPtr) {
        std::rethrow_exception(lstnExcPtr);
    }
    if (dpatchExcPtr) {
        std::rethrow_exception(dpatchExcPtr);
    }
}

void Listener::listen() {
    withFailureHandling([this](){
        do {
            transport_.listen();
        } while (not msgQueue_.closed() && continue_->load(std::memory_order_consume));
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

}  // namespace server
}  // namespace multio
