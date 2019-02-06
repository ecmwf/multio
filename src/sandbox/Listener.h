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

#ifndef multio_sandbox_Listener_H
#define multio_sandbox_Listener_H

#include <list>

#include "eckit/container/Queue.h"

#include "sandbox/Transport.h"

namespace multio {
namespace sandbox {

struct Connection {
    Connection(int id) : id_(id) {}
    int id_;
};
bool operator==(const Connection& lhs, const Connection& rhs) {
    return lhs.id_ == rhs.id_;
}

class Listener {
public:
    Listener(Transport& trans);

    void listen();

private:
    Transport& transport_;

    std::list<Connection> connections_;
    eckit::Queue<std::shared_ptr<Message>> msgQueue_{1024};
};

}  // namespace sandbox
}  // namespace multio

#endif
