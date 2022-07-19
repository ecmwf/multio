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
#include "multio/server/Dispatcher.h"
#include "multio/transport/Transport.h"

namespace multio {
namespace server {

using message::Message;
using util::ScopedThread;
using transport::Transport;

Listener::Listener(const eckit::Configuration& config, Transport& trans) :
    dispatcher_{std::make_shared<Dispatcher>(config)},
    transport_{trans},
    clientCount_{transport_.clientPeers().size()},
    msgQueue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE",1024*1024)) {}

void Listener::start() {

    eckit::ResourceUsage usage{"multio listener"};

    ScopedThread lstnThread{std::thread{&Listener::listen, this}};

    ScopedThread dpatchThread{std::thread{&Dispatcher::dispatch, dispatcher_, std::ref(msgQueue_)}};

    do {
        Message msg = transport_.receive();

        switch (msg.tag()) {
            case Message::Tag::Open:
                connections_.insert(msg.source());
                LOG_DEBUG_LIB(LibMultio)
                    << "*** OPENING connection to " << msg.source()
                    << ":    client count = " << clientCount_ << ", closed count = " << closedCount_
                    << ", connections = " << connections_.size() << std::endl;
                break;

            case Message::Tag::Close:
                connections_.erase(connections_.find(msg.source()));
                ++closedCount_;
                LOG_DEBUG_LIB(LibMultio)
                    << "*** CLOSING connection to " << msg.source()
                    << ":    client count = " << clientCount_ << ", closed count = " << closedCount_
                    << ", connections = " << connections_.size() << std::endl;
                break;

            case Message::Tag::Domain:
            case Message::Tag::Mask:
            case Message::Tag::StepNotification:
            case Message::Tag::StepComplete:
            case Message::Tag::Field:
                if(msg.metadata().has("domainCount")) {
                    ASSERT(msg.metadata().getUnsigned("domainCount") == clientCount_);
                }
                checkConnection(msg.source());
                LOG_DEBUG_LIB(LibMultio) << "*** Message received: " << msg << std::endl;
                msgQueue_.emplace(std::move(msg));
                break;

            default:
                std::ostringstream oss;
                oss << "Unhandled message: " << msg << std::endl;
                throw eckit::SeriousBug(oss.str());
        }
    } while (moreConnections());

    LOG_DEBUG_LIB(LibMultio) << "*** STOPPED listening loop " << std::endl;

    msgQueue_.close();

    LOG_DEBUG_LIB(LibMultio) << "*** CLOSED message queue " << std::endl;
}

void Listener::listen() {
    do {
        transport_.listen();
    } while (not msgQueue_.closed());
}

bool Listener::moreConnections() const {
    return !connections_.empty() || closedCount_ < clientCount_;
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
