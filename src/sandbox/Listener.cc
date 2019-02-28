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

#include <functional>

#include "eckit/config/Resource.h"

#include "sandbox/Mappings.h"
#include "sandbox/Message.h"
#include "sandbox/ScopedThread.h"

#include "sandbox/Dispatcher.h"
#include "sandbox/print_buffer.h"
#include "sandbox/Transport.h"

namespace multio {
namespace sandbox {

Listener::Listener(const eckit::Configuration& config, Transport& trans) :
    dispatcher_(std::make_shared<Dispatcher>(config)),
    transport_(trans),
    msgQueue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024)) {}

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
                break;

            case Message::Tag::Mapping:
                eckit::Log::info() << "*** MAPPING INDICES " << std::flush;
                print_buffer(static_cast<const size_t*>(msg.payload().data()),
                             msg.size() / sizeof(size_t), eckit::Log::info());
                eckit::Log::info() << std::endl;
                Mappings::instance().add(msg);
                break;

            default:
                eckit::Log::info() << "*** DISPATCH QUEUE " << std::flush;
                print_buffer(static_cast<const double*>(msg.payload().data()),
                             msg.size() / sizeof(double), eckit::Log::info());
                eckit::Log::info() << std::endl;
                msgQueue_.push(std::move(msg));
        }
    } while (not(connections_.empty()));

    // Wait for queue to be emptied before closing it
    while (not msgQueue_.empty()) {}

    msgQueue_.close();
}


}  // namespace sandbox
}  // namespace multio
