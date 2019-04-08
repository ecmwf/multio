/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "TcpTransport.h"

#include <algorithm>
#include <iostream>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/maths/Functions.h"
#include "eckit/runtime/Main.h"
#include "eckit/serialisation/MemoryStream.h"

namespace multio {
namespace sandbox {

TcpTransport::TcpTransport(const eckit::Configuration& config) :
    Transport(config),
    local_host_{eckit::Main::hostname()},
    local_port_{config.getUnsigned("local_port")} {

    auto serverConfigs = config.getSubConfigurations("servers");

    for (auto cfg : serverConfigs) {
        auto host = cfg.getString("host");
        auto ports = cfg.getUnsignedVector("ports");

        if (host == local_host_ && find(begin(ports), end(ports), local_port_) != end(ports)) {
            eckit::Log::info() << "Starting server(host=" << local_host_ << ", port=" << local_port_
                               << ")" << std::endl;

            server_.reset(new eckit::TCPServer{static_cast<int>(local_port_)});
            socket_ = server_->accept();
        }
        else {
            eckit::Log::info() << "Starting client(host=" << local_host_ << ", port=" << local_port_
                               << ")" << std::endl;

            for (const auto port : ports) {
                eckit::Log::info() << "Connecting to server(host=" << host << ", port=" << port
                                   << ")" << std::endl;
                try {
                    eckit::TCPClient client;
                    connections_.emplace(Peer{host, static_cast<size_t>(port)},
                                         new eckit::TCPSocket{client.connect(host, port, 5, 10)});
                } catch (eckit::TooManyRetries& e) {
                    eckit::Log::error() << "Failed to establish connection to host: " << host
                                        << ", port: " << port << std::endl;
                }
                eckit::Log::info() << "Number of connections: " << connections_.size() << std::endl;
            }
        }
    }
}

Message TcpTransport::receive() {
    size_t size;
    socket_.read(&size, sizeof(size));

    eckit::Log::info() << "Received size: " << size << std::endl;

    eckit::Buffer buffer{size};
    socket_.read(buffer, static_cast<long>(size));

    eckit::MemoryStream stream{buffer};
    Message msg;
    msg.decode(stream);

    eckit::Log::info() << "Received message content: "
                       << std::string(static_cast<char*>(msg.payload()), msg.size()) << std::endl;

    return msg;
}

void TcpTransport::send(const Message& msg) {
    auto dest = msg.destination();

    const auto& socket = connections_.at(msg.destination());

    // Add 4K for header/footer etc. Should be plenty
    eckit::Buffer buffer{eckit::round(msg.size(), 8) + 4096};

    eckit::MemoryStream stream{buffer};

    msg.encode(stream);

    auto size = stream.bytesWritten();
    eckit::Log::info() << "Sending size: " << size << std::endl;
    socket->write(&size, sizeof(size));

    eckit::Log::info() << "Sending message: " << msg << std::endl;
    socket->write(buffer, static_cast<int>(size));
}

Peer TcpTransport::localPeer() const {
    return Peer{local_host_, local_port_};
}

void TcpTransport::print(std::ostream& os) const {
    os << "TcpTransport()";
}

static TransportBuilder<TcpTransport> TcpTransportBuilder("Tcp");

}  // namespace sandbox
}  // namespace multio
