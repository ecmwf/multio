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
#include "sandbox/Peer.h"

namespace multio {
namespace sandbox {

class Listener {
public:
    Listener(Transport& trans);

    void listen();

private:

    Transport& transport_;

    std::list<Peer> connections_;
    eckit::Queue<Message> msgQueue_;
};

}  // namespace sandbox
}  // namespace multio

#endif
