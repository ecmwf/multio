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
#include <sstream>

#include "eckit/config/Resource.h"

#include "multio/transport/TransportRegistry.h"
#include "multio/util/Environment.h"

namespace multio::action::transport {

using message::Message;
using message::MetadataTypes;

namespace {
size_t serverIdDenom(size_t clientCount, size_t serverCount) {
    return (serverCount == 0) ? 1 : (((clientCount - 1) / serverCount) + 1);
}

std::vector<std::string> getHashKeys(const eckit::Configuration& conf) {
    if (conf.has("hash-keys")) {
        return conf.getStringVector("hash-keys");
    }
    return std::vector<std::string>{"category", "name", "level"};
}

}  // namespace

Transport::Transport(const ComponentConfiguration& compConf) :
    Action{compConf},
    transport_{multio::transport::TransportRegistry::instance().get(compConf)},
    client_{transport_->localPeer()},
    serverPeers_{transport_->serverPeers()},
    serverCount_{serverPeers_.size()},
    serverId_{client_.id() / serverIdDenom(transport_->serverCount(), serverCount_)},
    usedServerCount_{eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_USED_SERVERS", 1)},
    hashKeys_{getHashKeys(compConf.parsedConfig())},
    counters_(serverPeers_.size()),
    distType_{distributionType()} {}

void Transport::executeImpl(Message msg) {
    util::ScopedTiming timing{statistics_.actionTiming_};

    auto md = msg.metadata();
    if (md.get<bool>("toAllServers")) {
        for (auto& server : serverPeers_) {
            auto md = msg.metadata();
            Message trMsg{Message::Header{msg.tag(), client_, *server, std::move(md)}, msg.payload()};

            transport_->bufferedSend(trMsg);
        }
    }
    else {
        auto server = chooseServer(msg.metadata());

        Message trMsg{Message::Header{msg.tag(), client_, server, std::move(md)}, msg.payload()};

        transport_->bufferedSend(trMsg);
    }
}

void Transport::print(std::ostream& os) const {
    os << "Action[" << *transport_ << "]";
}

message::Peer Transport::chooseServer(const message::Metadata& metadata) {
    ASSERT_MSG(serverCount_ > 0, "No server to choose from");

    auto getMetadataValue = [&](const std::string& hashKey) -> const message::MetadataValue& {
        auto searchHashKey = metadata.find(hashKey);
        if (searchHashKey == metadata.end()) {
            std::ostringstream os;
            os << "The hash key \"" << hashKey << "\" is not defined in the metadata object: " << metadata << std::endl;
            throw multio::transport::TransportException(os.str(), Here());
        }
        return searchHashKey->second;
    };

    auto constructHash = [&]() {
        std::ostringstream os;

        for (const std::string& s : hashKeys_) {
            getMetadataValue(s).visit(eckit::Overloaded{
                [&s](const auto& v) -> util::IfTypeNotOf<decltype(v), MetadataTypes::Scalars> {
                    throw message::MetadataWrongTypeException(s, Here());
                },
                [&os](const auto& v) -> util::IfTypeOf<decltype(v), MetadataTypes::Scalars> { os << v; },
            });
        }
        return os.str();
    };

    switch (distType_) {
        case DistributionType::hashed_cyclic: {
            std::string hashString = constructHash();
            ASSERT(usedServerCount_ <= serverCount_);

            auto offset = std::hash<std::string>{}(hashString) % usedServerCount_;
            auto id = (serverId_ + offset) % serverCount_;

            ASSERT(id < serverPeers_.size());

            return *serverPeers_[id];
        }
        case DistributionType::hashed_to_single: {
            std::string hashString = constructHash();
            auto id = std::hash<std::string>{}(hashString) % serverCount_;

            ASSERT(id < serverPeers_.size());

            return *serverPeers_[id];
        }
        case DistributionType::even: {
            std::string hashString = constructHash();

            if (destinations_.find(hashString) != end(destinations_)) {
                return destinations_.at(hashString);
            }

            auto it = std::min_element(begin(counters_), end(counters_));
            auto id = static_cast<size_t>(std::distance(std::begin(counters_), it));

            ASSERT(id < serverPeers_.size());
            ASSERT(id < counters_.size());

            ++counters_[id];

            auto dest = *serverPeers_[id];
            destinations_[hashString] = *serverPeers_[id];

            return dest;
        }
        default:
            throw eckit::SeriousBug("Unhandled distribution type");
    }
}

Transport::DistributionType Transport::distributionType() {
    // std::map with transparent comparator std::less<> for string_view
    const std::map<std::string, enum DistributionType, std::less<>> str2dist
        = {{"hashed_cyclic", DistributionType::hashed_cyclic},
           {"hashed_to_single", DistributionType::hashed_to_single},
           {"even", DistributionType::even}};

    const char* envVar = "MULTIO_SERVER_DISTRIBUTION";
    auto key = util::getEnv(envVar);
    if (!key)
        return DistributionType::hashed_to_single;

    auto it = str2dist.find(*key);
    if (it == str2dist.end()) {
        std::ostringstream oss;
        oss << "Transport::distributionType(): Unsupported distribution type \"" << (*key)
            << "\" read from environment variable " << envVar << std::endl;
        throw multio::transport::TransportException(oss.str(), Here());
    }
    return it->second;
}

static ActionBuilder<Transport> TransportBuilder("transport");

}  // namespace multio::action::transport
