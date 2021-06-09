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

#include "multio/message/Metadata.h"
#include "multio/message/Peer.h"

namespace eckit {
class Buffer;
class Configuration;
}  // namespace eckit

namespace multio {

namespace server {

class Transport;

class MultioClient {
public:
    explicit MultioClient(const eckit::Configuration& config);

    ~MultioClient();

    void openConnections() const;

    void closeConnections() const;

    void sendDomain(message::Metadata metadata, eckit::Buffer&& domain);

    void sendField(message::Metadata metadata, eckit::Buffer&& field, bool to_all_servers = false);

    void sendStepComplete() const;

private:
    using PeerList = std::vector<std::unique_ptr<message::Peer>>;

    size_t clientCount_;
    size_t serverCount_;

    std::shared_ptr<Transport> transport_ = nullptr;

    const message::Peer client_;
    size_t serverId_;
    size_t usedServerCount_;
    PeerList serverPeers_;
};

}  // namespace server
}  // namespace multio

#endif
