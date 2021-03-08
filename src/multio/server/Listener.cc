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

#include "multio/domain/Mappings.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"

#include "multio/server/GribTemplate.h"
#include "multio/server/ScopedThread.h"

#include "multio/server/Dispatcher.h"
#include "multio/server/ThreadTransport.h"

namespace multio {
namespace server {

using message::Message;

Listener::Listener(const eckit::Configuration& config, Transport& trans) :
    dispatcher_{std::make_shared<Dispatcher>(config)},
    transport_{trans},
    msgQueue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024*1024)) {}

void Listener::listen() {
    ScopedThread scThread{std::thread{&Dispatcher::dispatch, dispatcher_, std::ref(msgQueue_)}};

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

            case Message::Tag::Grib:
                LOG_DEBUG_LIB(LibMultio)
                    << "*** Size of grib template: " << msg.size() << std::endl;
                GribTemplate::instance().add(msg);
                break;

            case Message::Tag::Domain:
                LOG_DEBUG_LIB(LibMultio)
                    << "*** Number of maps: " << msg.domainCount() << std::endl;
                checkConnection(msg.source());
                clientCount_ = msg.domainCount();
                domain::Mappings::instance().add(msg);
                break;

            case Message::Tag::StepNotification:
                LOG_DEBUG_LIB(LibMultio)
                    << "*** Step notification received from: " << msg.source() << std::endl;
                break;

            case Message::Tag::StepComplete:
                LOG_DEBUG_LIB(LibMultio)
                    << "*** Flush received from: " << msg.source() << std::endl;
                msgQueue_.push(std::move(msg));
                break;

            case Message::Tag::Field:
                checkConnection(msg.source());
                LOG_DEBUG_LIB(LibMultio)
                    << "*** Field received from: " << msg.source() << " with size "
                    << msg.size() / sizeof(double) << std::endl;
                msgQueue_.push(std::move(msg));
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

bool Listener::moreConnections() const {
    return !connections_.empty() || closedCount_ < clientCount_;
}

void Listener::checkConnection(const Peer& conn) const {
    if (connections_.find(conn) == end(connections_)) {
        throw eckit::SeriousBug("Connection is not open");
    }
}

}  // namespace server
}  // namespace multio
