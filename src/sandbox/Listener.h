/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#ifndef multio_sandbox_Listener_H
#define multio_sandbox_Listener_H

#include <list>
#include <memory>

#include "eckit/container/Queue.h"

#include "sandbox/Peer.h"
#include "sandbox/Message.h"

namespace eckit { class Configuration; }

namespace multio {
namespace sandbox {

class Transport;
class Dispatcher;

class Listener {
public:
    Listener(const eckit::Configuration& config, Transport& trans);

    void listen();

private:
    std::shared_ptr<Dispatcher> dispatcher_;

    Transport& transport_;

    size_t nbClosedConnections_ = 0;
    size_t nbMaps_ = 0;

    std::list<Peer> connections_;

    eckit::Queue<Message> msgQueue_;
};

}  // namespace sandbox
}  // namespace multio

#endif
