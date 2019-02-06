/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Listener.h"

#include <functional>

#include "sandbox/Message.h"
#include "sandbox/ScopedThread.h"

#include "sandbox/Dispatcher.h"
#include "sandbox/Transport.h"

namespace multio {
namespace sandbox {

Listener::Listener(Transport& trans) : transport_(trans) {}

void Listener::listen() {
    Dispatcher dispatcher;
    ScopedThread scThread{std::thread{&Dispatcher::dispatch, dispatcher, std::ref(msgQueue_)}};

    do {
        Message msg(0);
        transport_.receive(msg);

        switch (msg.tag()) {
            case MsgTag::open:
                connections_.push_back(Connection{msg.peer()});
                break;

            case MsgTag::close:
                connections_.remove(Connection{msg.peer()});
                break;

            default:
                msgQueue_.push(std::make_shared<Message>(std::move(msg)));
        }
    } while (not(connections_.empty()));

    // Wait for queue to be emptied before closing it
    while (not msgQueue_.empty()) {}

    msgQueue_.close();
}


}  // namespace sandbox
}  // namespace multio
