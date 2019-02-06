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

#ifndef multio_sandbox_Peer_H
#define multio_sandbox_Peer_H

#include <string>

namespace multio {
namespace sandbox {

class Peer {
public:
    Peer(const std::string& domain, int id);

    operator std::string();

    bool operator==(const Peer& rhs) { return id_ == rhs.id_ && domain_ == rhs.domain_; }

    bool operator!=(const Peer& rhs) { return not operator==(rhs); }

    bool operator<(const Peer& rhs) { return id_ < rhs.id_ && domain_ < rhs.domain_; }

private:  // methods
    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const Peer& x) {
        x.print(s);
        return s;
    }

private:
    int id_;  //< MPI rank or TCP port

    std::string domain_;  //< for MPI it will be Communicator, for TCP the host
};

}  // namespace sandbox
}  // namespace multio

#endif
