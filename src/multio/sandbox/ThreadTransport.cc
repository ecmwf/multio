
#include <iostream>

#include "ThreadTransport.h"

namespace multio {
namespace sandbox {

ThreadTransport::ThreadTransport(const eckit::LocalConfiguration& config) : Transport{config} {}
ThreadTransport::~ThreadTransport() = default;

void ThreadTransport::receive(Message& msg) {
    msg = internalBuffer_.pop();
}

void ThreadTransport::send(const Message& msg) {
    internalBuffer_.push(msg);
}

void ThreadTransport::print(std::ostream& os) const {
    os << "ThreadTransport[" << get_config_value<std::string>(config_, "name") << "]";
}

}  // namespace sandbox
}  // namespace multio
