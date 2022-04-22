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

#ifndef multio_server_actions_Transport_H
#define multio_server_actions_Transport_H

#include "multio/action/Action.h"
#include "multio/server/Transport.h" // This means circular dependency at the minute

namespace eckit { class Configuration; }

namespace multio {
namespace action {

using message::Message;

class TransportRegistry : public eckit::NonCopyable {
public:
    static TransportRegistry& instance();

    std::shared_ptr<server::Transport> get(const eckit::Configuration& config);

private:
    void add(const std::string& serverName);

    std::map<std::string, std::shared_ptr<server::Transport>> transports_;
    std::mutex mutex_;
};

// ================================================================================================

class Transport : public Action {
public:
    explicit Transport(const eckit::Configuration& config);

    void execute(Message msg) const override;

private:
    void print(std::ostream &os) const override;

    using PeerList = std::vector<std::unique_ptr<message::Peer>>;

    std::shared_ptr<server::Transport> transport_ = nullptr;

    const message::Peer client_;
    PeerList serverPeers_;

    size_t serverCount_;

    void setServerId(size_t clientCount) const;
    mutable size_t serverId_;
    size_t usedServerCount_;

    // Distribute fields
    message::Peer chooseServer(const message::Metadata& metadata) const;
    mutable std::map<std::string, message::Peer> destinations_;
    mutable std::vector<uint64_t> counters_;

    enum class DistributionType : unsigned
    {
        hashed_cyclic,
        hashed_to_single,
        even,
    };
    DistributionType distType_;

    enum DistributionType distributionType();
};

}  // namespace action
}  // namespace multio

#endif
