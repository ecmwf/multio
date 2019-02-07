/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_sandbox_Listener_H
#define multio_sandbox_Listener_H

#include "Dispatcher.h"

#include "eckit/log/Log.h"

namespace multio {
namespace sandbox {

Dispatcher::Dispatcher() {
}

void Dispatcher::dispatch(eckit::Queue<Message>& queue) {
    while(true) {
        Message msg;
        if(queue.pop(msg) < 0) {
            break;
        }
        eckit::Log::info() << static_cast<const char*>(msg.payload()) << std::endl;
    }
}

}  // namespace sandbox
}  // namespace multio

#endif
