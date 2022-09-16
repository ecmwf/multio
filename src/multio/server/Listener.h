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

#include <atomic>
#include <memory>
#include <set>

#include "eckit/container/Queue.h"

#include "multio/message/Message.h"
#include "multio/message/Peer.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/FailureHandling.h"

namespace eckit {
class Configuration;
}

namespace multio {

namespace transport {
class Transport;
}

namespace server {

class Dispatcher;

class Listener: public util::FailureAware<util::ComponentTag::Receiver> {
public:
    Listener(const util::ConfigurationContext& confCtx, transport::Transport& trans);

    void start();

    void listen();
    
    util::FailureHandlerResponse handleFailure(const eckit::Optional<util::OnReceiveError>&) override;

private:
    bool moreConnections() const;
    void checkConnection(const message::Peer& conn) const;

    std::shared_ptr<std::atomic<bool>> continue_;
    std::shared_ptr<Dispatcher> dispatcher_;

    transport::Transport& transport_;

    size_t closedCount_ = 0;
    size_t clientCount_ = 0;


    std::set<message::Peer> connections_;
    eckit::Queue<message::Message> msgQueue_;

};

}  // namespace server
}  // namespace multio

#endif
