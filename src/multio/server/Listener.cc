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

#include "multio/server/Mappings.h"
#include "multio/server/Message.h"
#include "multio/server/ScopedThread.h"

#include "multio/server/Dispatcher.h"
#include "multio/server/print_buffer.h"
#include "multio/server/ThreadTransport.h"

namespace multio {
namespace server {

Listener::Listener(const eckit::Configuration& config, Transport& trans) :
    dispatcher_{std::make_shared<Dispatcher>(config)},
    transport_{trans},
    msgQueue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024)) {
}

void Listener::listen() {

    ScopedThread scThread{std::thread{&Dispatcher::dispatch, dispatcher_, std::ref(msgQueue_)}};

    do {
        Message msg = transport_.receive();

        switch (msg.tag()) {
            case Message::Tag::Open:
                connections_.push_back(msg.source());
                eckit::Log::info() << "*** OPENING connection to " << msg.source() << std::endl;
                break;

            case Message::Tag::Close:
                connections_.remove(msg.source());
                eckit::Log::info() << "*** CLOSING connection to " << msg.source() << std::endl;
                ++nbClosedConnections_;
                break;

            case Message::Tag::GribTemplate:
                eckit::Log::info() << "*** Size of grib template: " << msg.size() << std::endl;
                break;

            case Message::Tag::Mapping:
                nbMaps_ = msg.map_count();
                Mappings::instance().add(msg);
                break;

            default:
                msgQueue_.push(std::move(msg));
        }
    } while (!connections_.empty() || nbClosedConnections_ != nbMaps_);

    msgQueue_.close();
}


}  // namespace server
}  // namespace multio
