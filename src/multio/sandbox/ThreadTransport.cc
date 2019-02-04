
#include <iostream>

#include "ThreadTransport.h"

namespace multio {
namespace sandbox {

ThreadTransport::ThreadTransport(const std::string& title) : Transport{title} {}
ThreadTransport::~ThreadTransport() = default;

void ThreadTransport::receive(Message& msg) {
    msg = internalBuffer_.front();
    internalBuffer_.pop();
}

void ThreadTransport::send(const Message& msg) {
    internalBuffer_.push(msg);
}

void ThreadTransport::print(std::ostream& os) const {
    os << "ThreadTransport[" << title_ << "]";
}

}  // namespace sandbox
}  // namespace multio
