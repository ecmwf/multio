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
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Mar 2019

#pragma once

#include <iosfwd>
#include <map>
#include <vector>

#include "eckit/io/Select.h"
#include "eckit/net/TCPClient.h"
#include "eckit/net/TCPServer.h"

#include "multio/transport/Transport.h"

namespace eckit {
class Configuration;
namespace net {
class TCPSocket;
}
}  // namespace eckit

namespace multio::transport {

class TcpPeer : public Peer {
public:
    TcpPeer(const std::string& host, size_t port);
    TcpPeer(const std::string& host, int port);

    const std::string& host() const;
    size_t port() const;
};

struct Connection;

class TcpTransport final : public Transport {
public:
    TcpTransport(const ComponentConfiguration& compConf);

private:
    void openConnections() override;
    void closeConnections() override;

    Message receive() override;

    void abort(std::exception_ptr) override;

    void send(const Message& message) override;

    void bufferedSend(const Message& msg) override;

    const Peer& localPeer() const override;

    PeerList createServerPeers() const override;

    void createPeers() const override;

    void print(std::ostream& os) const override;

    Message nextMessage(eckit::net::TCPSocket& socket) const;

    bool acceptConnection();
    void waitForEvent();

    bool amIServer(const std::string& host, std::vector<size_t> ports);

    TcpPeer local_;

    std::map<Peer, const std::unique_ptr<eckit::net::TCPSocket>> outgoing_;

    eckit::Select select_;

    std::unique_ptr<eckit::net::TCPServer> server_;
    std::vector<std::unique_ptr<Connection>> incoming_;
};

}  // namespace multio::transport
