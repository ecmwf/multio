
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
