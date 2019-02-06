/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <iostream>

#include "SimpleTransport.h"

#include "eckit/config/Resource.h"

namespace multio {
namespace sandbox {

SimpleTransport::SimpleTransport(const eckit::Configuration& config) :
    Transport{config},
    queue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024))
{
}

SimpleTransport::~SimpleTransport() = default;

Message SimpleTransport::receive() {
    return queue_.pop();
}

void SimpleTransport::send(const Message& msg) {
    queue_.push(msg);
}

Peer SimpleTransport::localPeer() const
{
    Peer peer("thread", std::hash<std::thread::id>{}(std::this_thread::get_id()));
    return peer;
}

void SimpleTransport::print(std::ostream& os) const {
    os << "SimpleTransport(name = " << name_ << ")";
}

}  // namespace sandbox
}  // namespace multio
