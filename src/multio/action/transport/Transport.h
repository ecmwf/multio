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

/// @date Jan 2022

#pragma once

#include "multio/action/Action.h"
#include "multio/transport/Transport.h"  // This means circular dependency at the minute

namespace eckit {
class Configuration;
}

namespace multio::action {

class Transport : public Action {
public:
    explicit Transport(const ComponentConfiguration& config);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;

    using PeerList = std::vector<std::unique_ptr<message::Peer>>;

    std::shared_ptr<transport::Transport> transport_ = nullptr;

    const message::Peer client_;
    const PeerList& serverPeers_;  // = transport_->serverPeers();

    size_t serverCount_;

    size_t serverId_;
    size_t usedServerCount_;

    std::vector<std::string> hashKeys_;

    // Distribute fields
    message::Peer chooseServer(const message::Metadata& metadata);
    std::map<std::string, message::Peer> destinations_;
    std::vector<uint64_t> counters_;

    enum class DistributionType : unsigned
    {
        hashed_cyclic,
        hashed_to_single,
        even,
    };
    DistributionType distType_;

    enum DistributionType distributionType();
};

}  // namespace multio::action
