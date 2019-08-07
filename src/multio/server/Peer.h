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

/// @date Jan 2019

#ifndef multio_server_Peer_H
#define multio_server_Peer_H

#include <string>

#include "multio/server/ScopedThread.h"

namespace multio {
namespace server {

class Peer {
public:
    Peer(const std::string& domain = "null", size_t id = 0) : domain_{domain}, id_{id} {}
    virtual ~Peer() = default;

    operator std::string();

    bool operator==(const Peer& rhs) const { return id_ == rhs.id_ && domain_ == rhs.domain_; }

    bool operator!=(const Peer& rhs) const { return not operator==(rhs); }

    bool operator<(const Peer& rhs) const {
        if(id_ != rhs.id_) {
            return id_ < rhs.id_;
        }
        return domain_ < rhs.domain_;
    }

    const std::string& domain() const { return domain_; }
    size_t id() const { return id_; }

protected:
    std::string domain_ ;
    size_t id_;

private:  // methods

    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const Peer& x) {
        x.print(s);
        return s;
    }
};


class ThreadPeer : public Peer {
public:
    ThreadPeer(std::thread t) :
        Peer{"thread", std::hash<std::thread::id>{}(t.get_id())},
        thread_{std::move(t)} {}

private:

    ScopedThread thread_;
};


class MpiPeer : public Peer {
public:
    MpiPeer(const std::string& comm, size_t rank) : Peer{comm, rank} {}
};


class TcpPeer : public Peer {
public:
    TcpPeer(const std::string& host, size_t port) : Peer{host, port} {}
    TcpPeer(const std::string& host, int port) : Peer{host, static_cast<size_t>(port)} {}

    const std::string& host() const { return domain_; }
    size_t port() const { return id_; }
};


}  // namespace server
}  // namespace multio

#endif
