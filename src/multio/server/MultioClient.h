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

#include "multio/server/Peer.h"

namespace eckit {
class Buffer;
class Configuration;
class LocalConfiguration;
}  // namespace eckit

namespace multio {
namespace server {

using Metadata = eckit::LocalConfiguration;
class Transport;
// class Peer;

class MultioClient {
public:
    explicit MultioClient(const eckit::Configuration& config);

    void openConnections() const;

    void closeConnections() const;

    void sendDomain(const std::string& name, const std::string& category, eckit::Buffer&& domain);

    void sendField(const std::string& name, const std::string& category, size_t gl_size,
                   const std::string& domain, const Metadata& metadata, eckit::Buffer&& field);

    void sendStepComplete() const;

private:
    using PeerList = std::vector<std::unique_ptr<Peer>>;

    Transport* createTransport(const eckit::Configuration& config);

    PeerList createServerPeers(const eckit::Configuration& config);

    size_t clientCount_;
    size_t serverCount_;

    std::shared_ptr<Transport> transport_ = nullptr;
    PeerList serverPeers_;

};

}  // namespace server
}  // namespace multio

#endif
