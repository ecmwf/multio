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

#ifndef multio_server_Listener_H
#define multio_server_Listener_H

#include <set>
#include <memory>

#include "eckit/container/Queue.h"

#include "multio/message/Peer.h"
#include "multio/message/Message.h"

namespace eckit {
class Configuration;
}

namespace multio {
namespace server {

class Transport;
class Dispatcher;

class Listener {
public:
    Listener(const eckit::Configuration& config, Transport& trans);

    void start();

    void listen();

private:
    bool moreConnections() const;
    void checkConnection(const message::Peer& conn) const;

    std::shared_ptr<Dispatcher> dispatcher_;

    Transport& transport_;

    size_t closedCount_ = 0;
    size_t clientCount_ = 0;

    std::set<message::Peer> connections_;

    eckit::Queue<message::Message> msgQueue_;
};

}  // namespace server
}  // namespace multio

#endif
