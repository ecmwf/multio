/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "TransportPeer.h"

#include <string>
#include <iostream>

namespace multio {
namespace server {

//--------------------------------------------------------------------------------------------------

ThreadPeer::ThreadPeer(std::thread t) :
    Peer{"thread", std::hash<std::thread::id>{}(t.get_id())},
    thread_{std::move(t)} {}


//--------------------------------------------------------------------------------------------------

MpiPeer::MpiPeer(const std::string& comm, size_t rank) : Peer{comm, rank} {}

//--------------------------------------------------------------------------------------------------

TcpPeer::TcpPeer(const std::string& host, size_t port) : Peer{host, port} {}
TcpPeer::TcpPeer(const std::string& host, int port) : Peer{host, static_cast<size_t>(port)} {}

const std::string& TcpPeer::host() const {
    return group_;
}

size_t TcpPeer::port() const {
    return id_;
}

}  // namespace server
}  // namespace multio
