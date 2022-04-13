
#include "MultioClient.h"

#include <algorithm>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/Resource.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/server/MpiTransport.h"
#include "multio/server/TcpTransport.h"

using multio::message::Peer;

namespace multio {
namespace server {

size_t serverIdDenom(size_t clientCount, size_t serverCount) {
    return (serverCount == 0) ? 1 : (((clientCount - 1) / serverCount) + 1);
}

MultioClient::MultioClient(const eckit::Configuration& config)
//    :
//    transport_(TransportFactory::instance().build(config.getString("transport"), config)),
//    client_{transport_->localPeer()},
//    serverId_{client_.id() / serverIdDenom(clientCount_, serverCount_)},
//    usedServerCount_{eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_USED_SERVERS", 1)},
//    serverPeers_{transport_->createServerPeers()},
//    counters_(serverPeers_.size()),
//    distType_{distributionType()}
{
    LOG_DEBUG_LIB(multio::LibMultio) << "Client config: " << config << std::endl;
    const std::vector<eckit::LocalConfiguration> plans =
        config.getSubConfiguration("client").getSubConfigurations("plans");
    for (const auto& cfg : plans) {
        eckit::Log::debug<LibMultio>() << cfg << std::endl;
        plans_.emplace_back(new action::Plan(cfg));
    }
}

MultioClient::~MultioClient() = default;

void MultioClient::openConnections() const {
    // transport_->openConnections();

    Message msg{Message::Header{Message::Tag::Open, Peer{}, Peer{}}};
    for (const auto& plan : plans_) {
        plan->process(msg);
    }
}

void MultioClient::closeConnections() const {
    // transport_->closeConnections();

    Message msg{Message::Header{Message::Tag::Close, Peer{}, Peer{}}};
    for (const auto& plan : plans_) {
        plan->process(msg);
    }
}

//void MultioClient::sendDomain(message::Metadata metadata, eckit::Buffer&& domain) {
//    for (auto& server : serverPeers_) {
//        Message msg{Message::Header{Message::Tag::Domain, client_, *server, std::move(metadata)},
//                    domain};

//        transport_->bufferedSend(msg);
//    }
//}

//void MultioClient::sendMask(message::Metadata metadata, eckit::Buffer&& mask) {
//    for (auto& server : serverPeers_) {
//        Message msg{Message::Header{Message::Tag::Mask, client_, *server, std::move(metadata)},
//                    mask};

//        transport_->bufferedSend(msg);
//    }
//}

//void MultioClient::sendField(message::Metadata metadata, eckit::Buffer&& field,
//                             bool to_all_servers) {
//    if (to_all_servers) {
//        for (auto& server : serverPeers_) {
//            Message msg{Message::Header{Message::Tag::Field, client_, *server, std::move(metadata)},
//                        field};

//            transport_->bufferedSend(msg);
//        }
//    }
//    else {
//        auto server = chooseServer(metadata);

//        Message msg{
//            Message::Header{Message::Tag::Field, client_, server, std::move(metadata)},
//            std::move(field)};

//        transport_->bufferedSend(msg);
//    }
//}

void MultioClient::dispatch(message::Metadata metadata, eckit::Buffer&& payload, int itag) {
    auto tag = static_cast<Message::Tag>(itag);
    ASSERT(tag < Message::Tag::ENDTAG);
    Message msg{Message::Header{tag, Peer{}, Peer{}, std::move(metadata)},
                std::move(payload)};

    for (const auto& plan : plans_) {
        plan->process(msg);
    }
}

void MultioClient::dispatch(message::Message msg) {
    for (const auto& plan : plans_) {
        plan->process(msg);
    }
}

//void MultioClient::sendStepComplete() const {
//    for (auto& server : serverPeers_) {
//        Message msg{Message::Header{Message::Tag::StepComplete, client_, *server}};
//        transport_->bufferedSend(msg);
//    }
//}

//message::Peer MultioClient::chooseServer(const message::Metadata& metadata) {
//    ASSERT_MSG(serverCount_ > 0, "No server to choose from");

//    switch (distType_) {
//        case DistributionType::hashed_cyclic: {
//            std::ostringstream os;
//            os << metadata.getString("category") << metadata.getString("nemoParam")
//               << metadata.getString("param") << metadata.getLong("level");

//            ASSERT(usedServerCount_ <= serverCount_);

//            auto offset = std::hash<std::string>{}(os.str()) % usedServerCount_;
//            auto id = (serverId_ + offset) % serverCount_;

//            ASSERT(id < serverPeers_.size());

//            return *serverPeers_[id];
//        }
//        case DistributionType::hashed_to_single: {
//            std::ostringstream os;
//            os << metadata.getString("category") << metadata.getString("nemoParam")
//               << metadata.getString("param") << metadata.getLong("level");

//            auto id = std::hash<std::string>{}(os.str()) % serverCount_;

//            ASSERT(id < serverPeers_.size());

//            return *serverPeers_[id];
//        }
//        case DistributionType::even: {
//            std::ostringstream os;
//            os << metadata.getString("category") << metadata.getString("nemoParam")
//               << metadata.getString("param") << metadata.getLong("level");

//            if (destinations_.find(os.str()) != end(destinations_)) {
//                return destinations_.at(os.str());
//            }

//            auto it = std::min_element(begin(counters_), end(counters_));
//            auto id = static_cast<size_t>(std::distance(std::begin(counters_), it));

//            ASSERT(id < serverPeers_.size());
//            ASSERT(id < counters_.size());

//            ++counters_[id];

//            auto dest = *serverPeers_[id];
//            destinations_[os.str()] = *serverPeers_[id];

//            return dest;
//        }
//        default:
//            throw eckit::SeriousBug("Unhandled distribution type");
//    }
//}

//MultioClient::DistributionType MultioClient::distributionType() {
//    const std::map<std::string, enum DistributionType> str2dist = {
//        {"hashed_cyclic", DistributionType::hashed_cyclic},
//        {"hashed_to_single", DistributionType::hashed_to_single},
//        {"even", DistributionType::even}};

//    auto key = std::getenv("MULTIO_SERVER_DISTRIBUTION");
//    return key ? str2dist.at(key) : DistributionType::hashed_to_single;
//}

}  // namespace server
}  // namespace multio
