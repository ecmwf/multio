/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Transport.h"

#include <algorithm>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/config/Resource.h"

#include "multio/server/ConfigurationPath.h"
#include "multio/server/Transport.h"
#include "multio/util/logfile_name.h"

namespace multio {
namespace action {

namespace {
std::shared_ptr<server::Transport> make_transport(const eckit::Configuration &config) {
    eckit::Log::info() << "Action transport config: " << config << std::endl;
    auto serverName = config.getString("target");
    eckit::LocalConfiguration fullConfig{eckit::YAMLConfiguration{configuration_file()}};
    auto serverConfig = fullConfig.getSubConfiguration(serverName);
    return std::shared_ptr<server::Transport>{server::TransportFactory::instance().build(
        serverConfig.getString("transport"), serverConfig)};
}

size_t serverIdDenom(size_t clientCount, size_t serverCount) {
    return (serverCount == 0) ? 1 : (((clientCount - 1) / serverCount) + 1);
}
}  // namespace

Transport::Transport(const eckit::Configuration& config) :
    Action{config},
    transport_{make_transport(config)},
    client_{transport_->localPeer()},
    serverPeers_{transport_->createServerPeers()},
    serverCount_{serverPeers_.size()},
    usedServerCount_{eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_USED_SERVERS", 1)},
    counters_(serverPeers_.size()),
    distType_{distributionType()} {}

void Transport::execute(Message msg) const {
    eckit::Log::info() << "Execute transport action for message " << msg << std::endl;
    if (msg.tag() == Message::Tag::Open) {
        eckit::Log::info() << "Opening connections " << std::endl;
        setServerId(msg.metadata().getUnsigned("clientCount"));
        transport_->openConnections();
        return;
    }

    if (msg.tag() == Message::Tag::Close) {
        transport_->closeConnections();
        return;
    }

    auto md = msg.metadata();
    if (md.getBool("toAllServers")) {
        for (auto& server : serverPeers_) {
            auto md = msg.metadata();
            Message trMsg{Message::Header{msg.tag(), client_, *server, std::move(md)},
                          msg.payload()};

            transport_->bufferedSend(trMsg);
        }
    }
    else {
        auto server = chooseServer(msg.metadata());

        Message trMsg{Message::Header{msg.tag(), client_, server, std::move(md)},
                    std::move(msg.payload())};

        transport_->bufferedSend(trMsg);
    }
}

void Transport::print(std::ostream& os) const {
    os << "Action[" << *transport_ << "]";
}

void Transport::setServerId(size_t clientCount) const {
    serverId_ = client_.id() / serverIdDenom(clientCount, serverCount_);
}

message::Peer Transport::chooseServer(const message::Metadata& metadata) const {
    ASSERT_MSG(serverCount_ > 0, "No server to choose from");

    switch (distType_) {
        case DistributionType::hashed_cyclic: {
            std::ostringstream os;
            os << metadata.getString("category") << metadata.getString("nemoParam")
               << metadata.getString("param") << metadata.getLong("level");

            ASSERT(usedServerCount_ <= serverCount_);

            auto offset = std::hash<std::string>{}(os.str()) % usedServerCount_;
            auto id = (serverId_ + offset) % serverCount_;

            ASSERT(id < serverPeers_.size());

            return *serverPeers_[id];
        }
        case DistributionType::hashed_to_single: {
            std::ostringstream os;
            os << metadata.getString("category") << metadata.getString("nemoParam")
               << metadata.getString("param") << metadata.getLong("level");

            auto id = std::hash<std::string>{}(os.str()) % serverCount_;

            ASSERT(id < serverPeers_.size());

            return *serverPeers_[id];
        }
        case DistributionType::even: {
            std::ostringstream os;
            os << metadata.getString("category") << metadata.getString("nemoParam")
               << metadata.getString("param") << metadata.getLong("level");

            if (destinations_.find(os.str()) != end(destinations_)) {
                return destinations_.at(os.str());
            }

            auto it = std::min_element(begin(counters_), end(counters_));
            auto id = static_cast<size_t>(std::distance(std::begin(counters_), it));

            ASSERT(id < serverPeers_.size());
            ASSERT(id < counters_.size());

            ++counters_[id];

            auto dest = *serverPeers_[id];
            destinations_[os.str()] = *serverPeers_[id];

            return dest;
        }
        default:
            throw eckit::SeriousBug("Unhandled distribution type");
    }
}

Transport::DistributionType Transport::distributionType() {
    const std::map<std::string, enum DistributionType> str2dist = {
        {"hashed_cyclic", DistributionType::hashed_cyclic},
        {"hashed_to_single", DistributionType::hashed_to_single},
        {"even", DistributionType::even}};

    auto key = std::getenv("MULTIO_SERVER_DISTRIBUTION");
    return key ? str2dist.at(key) : DistributionType::hashed_to_single;
}

static ActionBuilder<Transport> TransportBuilder("Transport");

}  // namespace action
}  // namespace multio
