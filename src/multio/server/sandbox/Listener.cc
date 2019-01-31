
#include "Listener.h"

#include <functional>

#include "multio/server/Message.h"
#include "multio/server/msg_tag.h"
#include "multio/server/ScopedThread.h"

#include "multio/server/sandbox/Dispatcher.h"
#include "multio/server/sandbox/Transport.h"

namespace multio {
namespace server {
namespace sandbox {


Listener::Listener(Transport& trans) : transport_(trans) {
}

void Listener::listen() {

    Dispatcher dispatcher;
    ScopedThread scThread{std::thread{&Dispatcher::dispatch, dispatcher, std::ref(msgQueue_)}};

    do {
         Message msg(0);
         transport_.receive(msg);

         switch (msg.tag()) {
             case msg_tag::open:
                 connections_.push_back(Connection{msg.peer()});
                 break;

             case msg_tag::close:
                 connections_.remove(Connection{msg.peer()});
                 break;

             default:
                 msgQueue_.push(std::make_shared<Message>(std::move(msg)));
         }
     } while (not (connections_.empty()));

    // Wait for queue to be emptied before closing it
    while(not msgQueue_.empty()) {
        ;
    }

    msgQueue_.close();
}


}  // namespace sandbox
}  // namespace server
}  // namespace multio
