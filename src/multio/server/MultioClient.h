/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_server_MultioClient_H
#define multio_server_MultioClient_H

#include <memory>
#include <vector>

#include "eckit/log/JSON.h"

#include <multio/server/Message.h>
#include <multio/server/Peer.h>
#include <multio/server/Transport.h>

namespace eckit {
class Configuration;
}  // namespace eckit

namespace multio {
namespace server {

class MultioClient {
public:
    MultioClient(const eckit::Configuration& config);

    void openConnections() const;

    void closeConnections() const;

    void sendDomain(const std::string& name, const std::string& category, eckit::Buffer&& domain);

    void sendField(const std::string& name, const std::string& category, size_t gl_size,
                   const std::string& domain, const Metadata& metadata, eckit::Buffer&& field);

private:
    using PeerList = std::vector<std::unique_ptr<Peer>>;

    Transport* createTransport(const eckit::Configuration& config);

    PeerList createServerPeers(const eckit::Configuration& config);

    std::unique_ptr<Transport> transport_;
    PeerList serverPeers_;

    size_t clientCount_;
    size_t serverCount_;
};

MultioClient::MultioClient(const eckit::Configuration& config) :
    transport_(createTransport(config)),
    serverPeers_{createServerPeers(config)} {}

void MultioClient::openConnections() const {
    auto client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::Open, client, *server}, std::string("open")};
        transport_->send(msg);
    }
}

void MultioClient::closeConnections() const {
    auto client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::Open, client, *server}, std::string("close")};
        transport_->send(msg);
    }
}

void MultioClient::sendDomain(const std::string& name, const std::string& category,
                              eckit::Buffer&& domain) {
    Peer client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{
            Message::Header{Message::Tag::Mapping, client, *server, name, category, clientCount_},
            std::move(domain)};

        transport_->send(msg);
    }
}

void MultioClient::sendField(const std::string& name, const std::string& category, size_t gl_size,
                             const std::string& domain, const Metadata& metadata,
                             eckit::Buffer&& field) {
    Peer client = transport_->localPeer();

    std::stringstream field_id;
    eckit::JSON json(field_id);
    json << metadata;

    // Choose server
    auto id = std::hash<std::string>{}(field_id.str()) % serverCount_;
    ASSERT(id < serverPeers_.size());

    Message msg{Message::Header{Message::Tag::Field, client, *serverPeers_[id], name, category,
                                clientCount_, gl_size, domain, field_id.str()},
                std::move(field)};

    transport_->send(msg);
}

Transport* MultioClient::createTransport(const eckit::Configuration& config) {
    return TransportFactory::instance().build(config.getString("transport"), config);
}

MultioClient::PeerList MultioClient::createServerPeers(const eckit::Configuration& config) {
    PeerList serverPeers;

    std::string transport = config.getString("transport");
    std::string group = config.getString("group");

    // For MPI
    if (transport == "mpi") {
        auto comm_size = clientCount_ + serverCount_;
        auto rank = clientCount_;
        while (rank != comm_size) {
            serverPeers.emplace_back(new MpiPeer{group, rank++});
        }

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


}  // namespace server
}  // namespace multio

#endif
