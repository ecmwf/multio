
#include "MultioClient.h"

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
    serverPeers_{createServerPeers(config)} {
    eckit::Log::debug<multio::LibMultio>() << config << std::endl;
}

MultioClient::~MultioClient() = default;

void MultioClient::openConnections() const {
    auto client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::Open, client, *server}};
        transport_->send(msg);
    }
}

void MultioClient::closeConnections() const {
    auto client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::Close, client, *server}};
        transport_->send(msg);
    }
}

void MultioClient::sendDomain(message::Metadata metadata, eckit::Buffer&& domain) {
    Peer client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::Domain, client, *server, std::move(metadata)},
                    std::move(domain)};

        transport_->send(msg);
    }
}

void MultioClient::sendField(message::Metadata metadata, eckit::Buffer&& field,
                             bool to_all_servers) {
    Peer client = transport_->localPeer();
    std::string field_id = message::to_string(metadata);

    if (to_all_servers) {
        for (auto& server : serverPeers_) {
            Message msg{Message::Header{Message::Tag::Field, client, *server, std::move(metadata)},
                        std::move(field)};

            transport_->send(msg);
        }
    }
    else {
        // Choose server
        std::ostringstream os;
        os << metadata.getString("category") << metadata.getString("nemoParam")
           << metadata.getString("param") << metadata.getLong("level");
        auto id = std::hash<std::string>{}(os.str()) % serverCount_;
        ASSERT(id < serverPeers_.size());

        Message msg{
            Message::Header{Message::Tag::Field, client, *serverPeers_[id], std::move(metadata)},
            std::move(field)};

        transport_->send(msg);
    }
}

void MultioClient::sendStepComplete() const {
    auto client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::StepComplete, client, *server}};
        transport_->send(msg);
    }
}


MultioClient::PeerList MultioClient::createServerPeers(const eckit::Configuration& config) {
    PeerList serverPeers;

    std::string transport = config.getString("transport");
    std::string group = config.getString("group");

    eckit::Log::debug<multio::LibMultio>()
        << "transport = " << transport << ", group = " << group << ", clients = " << clientCount_
        << ", servers = " << serverCount_ << std::endl;

    // TODO: May want to move this part inside Transport to avoid these conditions

    // For MPI -- this is dangerous as it requires having the same logic as in NEMO or IFS
    // Perhpas you want ot create an intercommunicator
    // Move this to the transport layer -- that should create the peerList and pass it back
    if (transport == "mpi") {
        auto comm_size = clientCount_ + serverCount_;
        auto rank = clientCount_;
        while (rank != comm_size) {
            serverPeers.emplace_back(new MpiPeer{group, rank++});
        }

        eckit::Log::debug<multio::LibMultio>()
            << "Returning list of " << serverPeers.size() << " server"
            << (serverPeers.size() > 1 ? "s" : "") << std::endl;

        return serverPeers;
    }

    // For TCP
    for (auto cfg : config.getSubConfigurations("servers")) {
        auto host = cfg.getString("host");
        for (auto port : cfg.getUnsignedVector("ports")) {
            serverPeers.emplace_back(new TcpPeer{host, port});
        }
    }
    return serverPeers;
}

}
}
