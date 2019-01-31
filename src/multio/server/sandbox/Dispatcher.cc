
#ifndef multio_server_sandbox_Listener_H
#define multio_server_sandbox_Listener_H

#include "Dispatcher.h"

#include <fstream>

#include "multio/server/Message.h"

namespace multio {
namespace server {
namespace sandbox {

namespace {
std::fstream fout("test_output", fout.out);
void process(const Message& msg) {
    fout << msg;
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
}  // namespace server
}  // namespace multio

#endif
