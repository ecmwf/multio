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
#include "eckit/log/Plural.h"
#include "eckit/maths/Functions.h"
#include "eckit/runtime/Main.h"
#include "eckit/serialisation/MemoryStream.h"

namespace multio {
namespace sandbox {

struct Connection {
    eckit::Select& select_;
    eckit::TCPSocket socket_;

    Connection(eckit::Select& select, eckit::TCPSocket& socket) : select_{select}, socket_{socket} {
        select_.add(socket_);
    }

    ~Connection() {
        select_.remove(socket_);
        socket_.close();
    }

    bool ready() { return select_.set(socket_); }
};

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
            select_.add(*server_);
        }
        else {
            // TODO: assert that (local_host_, local_port_) is in the list of clients
            eckit::Log::info() << "Starting client(host=" << local_host_ << ", port=" << local_port_
                               << ")" << std::endl;

            for (const auto port : ports) {
                eckit::Log::info() << "Connecting to server(host=" << host << ", port=" << port
                                   << ")" << std::endl;
                try {
                    eckit::TCPClient client;
                    outgoing_.emplace(Peer{host, static_cast<size_t>(port)},
                                         new eckit::TCPSocket{client.connect(host, port, 5, 10)});
                } catch (eckit::TooManyRetries& e) {
                    eckit::Log::error() << "Failed to establish connection to host: " << host
                                        << ", port: " << port << std::endl;
                }
                eckit::Log::info() << "Number of outgoing connections: " << outgoing_.size() << std::endl;
            }
        }
    }
}

Message TcpTransport::nextMessage(eckit::TCPSocket& socket) const {
    Message msg;

    size_t size;
    socket.read(&size, sizeof(size));

    eckit::Log::info() << "Received size: " << size << std::endl;

    eckit::Buffer buffer{size};
    socket.read(buffer, static_cast<long>(size));

    eckit::MemoryStream stream{buffer};
    msg.decode(stream);

    return msg;
}

Message TcpTransport::receive() {

    waitForEvent();

    for (auto it = begin(incoming_); it != end(incoming_); ++it) {
        if (not (*it)->ready()) {
            continue;
        }

        auto msg = nextMessage((*it)->socket_);

        eckit::Log::info() << "Received message: " << msg << std::endl;

        if(msg.tag() == Message::Tag::Close) {
            incoming_.erase(it);
        } else {
            std::swap(*it, incoming_[incoming_.size() - 1]);
        }

        return msg;
    }

    throw eckit::SeriousBug("No message received");
}

void TcpTransport::send(const Message& msg) {
    auto dest = msg.destination();

    const auto& socket = outgoing_.at(msg.destination());

    // Add 4K for header/footer etc. Should be plenty
    eckit::Buffer buffer{eckit::round(msg.size(), 8) + 4096};

    eckit::MemoryStream stream{buffer};

    msg.encode(stream);

    auto size = stream.bytesWritten();
    eckit::Log::info() << "Sending size: " << size << std::endl;
    socket->write(&size, sizeof(size));

    eckit::Log::info() << "Sending: " << msg << std::endl;
    socket->write(buffer, static_cast<int>(size));
}

Peer TcpTransport::localPeer() const {
    return Peer{local_host_, local_port_};
}

void TcpTransport::print(std::ostream& os) const {
    os << "TcpTransport()";
}

bool TcpTransport::acceptConnection() {
    if (not select_.set(*server_)) {
        return false;
    }

    eckit::TCPSocket socket{server_->accept()};
    incoming_.emplace_back(new Connection{select_, socket});

    return true;
}

void TcpTransport::waitForEvent() {
    do {
        while (not select_.ready(5)) {
            eckit::Log::info() << "Waiting... There are "
                               << eckit::Plural(incoming_.size(), "connection") << " still active"
                               << std::endl;
        }
    } while (acceptConnection());
}

static TransportBuilder<TcpTransport> TcpTransportBuilder("Tcp");

}  // namespace sandbox
}  // namespace multio
