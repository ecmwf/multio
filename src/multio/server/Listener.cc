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

#include <fstream>
#include <functional>
#include <typeinfo>

#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"

#include "multio/server/GribTemplate.h"
#include "multio/server/Mappings.h"
#include "multio/server/Message.h"
#include "multio/server/ScopedThread.h"

#include "multio/server/Dispatcher.h"
#include "multio/server/ThreadTransport.h"

namespace multio {
namespace server {

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
                eckit::Log::debug<LibMultio>()
                    << "*** OPENING connection to " << msg.source() << std::endl;
                break;

            case Message::Tag::Close:
                connections_.erase(connections_.find(msg.source()));
                eckit::Log::debug<LibMultio>()
                    << "*** CLOSING connection to " << msg.source() << std::endl;
                ++closedCount_;
                break;

            case Message::Tag::Grib:
                eckit::Log::debug<LibMultio>()
                    << "*** Size of grib template: " << msg.size() << std::endl;
                GribTemplate::instance().add(msg);
                break;

            case Message::Tag::Domain:
                checkConnection(msg.source());
                eckit::Log::debug<LibMultio>()
                    << "*** Number of maps: " << msg.domainCount() << std::endl;
                clientCount_ = msg.domainCount();
                Mappings::instance().add(msg);
                break;

            case Message::Tag::StepNotification:
                eckit::Log::debug<LibMultio>()
                    << "*** Step notification received from: " << msg.source() << std::endl;
                break;

            case Message::Tag::StepComplete:
                eckit::Log::debug<LibMultio>()
                    << "*** Flush received from: " << msg.source() << std::endl;
                msgQueue_.push(std::move(msg));
                break;

            case Message::Tag::Field:
                checkConnection(msg.source());
                eckit::Log::debug<LibMultio>()
                    << "*** Field received from: " << msg.source() << std::endl;
                eckit::Log::debug<LibMultio>()
                    << "    Size of payload: " << msg.size() << std::endl;
                eckit::Log::debug<LibMultio>()
                    << "    Size of   field: " << msg.size() / sizeof(double) << std::endl;
                msgQueue_.push(std::move(msg));
                break;

            default:
                std::ostringstream oss;
                oss << "Unhandled message: " << msg << std::endl;
                throw eckit::SeriousBug(oss.str());
        }
    } while (moreConnections());

    msgQueue_.close();
}

bool Listener::moreConnections() const {
    return !connections_.empty() || closedCount_ != clientCount_;
}

void Listener::checkConnection(const Peer& conn) const {
    if (connections_.find(conn) == end(connections_)) {
        throw eckit::SeriousBug("Connection is not open");
    }
}

}  // namespace server
}  // namespace multio
