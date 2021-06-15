
#include "MultioClient.h"

#include <algorithm>

#include "eckit/config/Resource.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/server/MpiTransport.h"
#include "multio/server/TcpTransport.h"
#include "multio/util/print_buffer.h"

using multio::message::Peer;

namespace multio {
namespace server {

MultioClient::MultioClient(const eckit::Configuration& config) :
    clientCount_{config.getUnsigned("clientCount")},
    serverCount_{config.getUnsigned("serverCount")},
    transport_(TransportFactory::instance().build(config.getString("transport"), config)),
    client_{transport_->localPeer()},
    serverId_{client_.id() / (((clientCount_ - 1) / serverCount_) + 1)},
    usedServerCount_{eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_USED_SERVERS", 1)},
    serverPeers_{transport_->createServerPeers()},
    counters_(serverPeers_.size()),
    distType_{distributionType()} {
    eckit::Log::debug<multio::LibMultio>() << config << std::endl;
}

MultioClient::~MultioClient() = default;

void MultioClient::openConnections() const {
    transport_->openConnections();
}

void MultioClient::closeConnections() const {
    transport_->closeConnections();
}

void MultioClient::sendDomain(message::Metadata metadata, eckit::Buffer&& domain) {
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::Domain, client_, *server, std::move(metadata)},
                    domain};

        transport_->bufferedSend(msg);
    }
}

void MultioClient::sendField(message::Metadata metadata, eckit::Buffer&& field,
                             bool to_all_servers) {

    if (to_all_servers) {
        for (auto& server : serverPeers_) {
            Message msg{Message::Header{Message::Tag::Field, client_, *server, std::move(metadata)},
                        field};

            transport_->bufferedSend(msg);
        }
    }
    else {
        auto server = chooseServer(metadata);

//        eckit::Log::info() << " *** Server " << server << " is picked for field "
//                           << message::to_string(metadata) << std::endl;

        Message msg{
            Message::Header{Message::Tag::Field, client_, server, std::move(metadata)},
            std::move(field)};

        transport_->bufferedSend(msg);
    }
}

void MultioClient::sendStepComplete() const {
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::StepComplete, client_, *server}};
        transport_->bufferedSend(msg);
    }
}

message::Peer MultioClient::chooseServer(const message::Metadata& metadata) {
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

MultioClient::DistributionType MultioClient::distributionType() {
    const std::map<std::string, enum DistributionType> str2dist = {
        {"hashed_cyclic", DistributionType::hashed_cyclic},
        {"hashed_to_single", DistributionType::hashed_to_single},
        {"even", DistributionType::even}};

    std::string key = ::getenv("MULTIO_SERVER_DISTRIBUTION");

    eckit::Log::info() << " *** Setting distribution type to " << key << " -- "
                       << static_cast<unsigned>(str2dist.at(key)) << std::endl;

    return (key == "") ? DistributionType::hashed_to_single : str2dist.at(key);
}

}  // namespace server
}  // namespace multio
