
#include "MultioClient.h"

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

MultioClient::MultioClient(const eckit::Configuration& config) :
    clientCount_{config.getUnsigned("clientCount")},
    serverCount_{config.getUnsigned("serverCount")},
    transport_(TransportFactory::instance().build(config.getString("transport"), config)),
    client_{transport_->localPeer()},
    serverId_{client_.id() / (((clientCount_ - 1) / serverCount_) + 1)},
    usedServerCount_{eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_USED_SERVERS", 1)},
    serverPeers_{transport_->createServerPeers()} {
    ASSERT(usedServerCount_ <= serverCount_);
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
    std::string field_id = message::to_string(metadata);

    if (to_all_servers) {
        for (auto& server : serverPeers_) {
            Message msg{Message::Header{Message::Tag::Field, client_, *server, std::move(metadata)},
                        field};

            transport_->bufferedSend(msg);
        }
    }
    else {
        // Choose server
        std::ostringstream os;
        os << metadata.getString("category") << metadata.getString("nemoParam")
           << metadata.getString("param") << metadata.getLong("level");

        auto id = std::hash<std::string>{}(os.str()) % serverCount_;

//        auto offset = std::hash<std::string>{}(os.str()) % usedServerCount_;
//        auto id = (serverId_ + offset) % serverCount_;

        ASSERT(id < serverPeers_.size());

        //        eckit::Log::info() << " ***** " << client_ << ": destination id = " << id
        //                           << ", destination = " << *serverPeers_[id] << std::endl;

        Message msg{
            Message::Header{Message::Tag::Field, client_, *serverPeers_[id], std::move(metadata)},
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

}
}
