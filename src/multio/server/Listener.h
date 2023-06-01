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

#pragma once

#include <atomic>
#include <memory>
#include <set>

#include "eckit/container/Queue.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/message/Peer.h"
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

struct ReceiverFailureTraits {
    using OnErrorType = util::OnReceiveError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence = util::integer_sequence<OnErrorType, OnErrorType::Propagate>;
    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnErrorType::Propagate; };
    static inline std::string configKey() { return std::string("on-receive-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    };
    static inline std::string componentName() { return std::string("Receiver"); };
};


class Listener : public util::FailureAware<ReceiverFailureTraits> {
public:
    Listener(const config::ComponentConfiguration& compConf, transport::Transport& trans);
    ~Listener();

    void start();

    void listen();

    util::FailureHandlerResponse handleFailure(util::OnReceiveError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

private:
    bool moreConnections() const;
    void checkConnection(const message::Peer& conn) const;

    std::unique_ptr<Dispatcher> dispatcher_;

    transport::Transport& transport_;

    size_t openedCount_ = 0;
    size_t clientCount_ = 0;


    std::set<message::Peer> connections_;
    mutable eckit::Queue<message::Message>
        msgQueue_;  // Mark mutable to be able to close when handling failure in const function
};

}  // namespace server
}  // namespace multio
