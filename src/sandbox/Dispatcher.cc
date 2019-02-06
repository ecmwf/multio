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

#include <fstream>

namespace multio {
namespace sandbox {

namespace {
std::fstream fout("test_output", fout.out);
void process(const Message& msg) {
    fout << msg << std::endl;
}
}

Dispatcher::Dispatcher() {
}

void Dispatcher::dispatch(eckit::Queue<std::shared_ptr<Message>>& queue) {
    do {
        auto msg = queue.pop();
        process(*msg);
    } while(not queue.closed());
}

}  // namespace sandbox
}  // namespace multio

#endif
