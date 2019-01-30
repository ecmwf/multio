
#include <iostream>

#include "DummyTransport.h"

#include "multio/server/Message.h"

namespace multio {
namespace server {
namespace sandbox {

DummyTransport::DummyTransport(const std::string& title) : Transport{title} {}
DummyTransport::~DummyTransport() = default;

void DummyTransport::receive(Message& msg) {
    msg = internalBuffer_.front();
    internalBuffer_.pop();
}

void DummyTransport::send(const Message& msg) {
    internalBuffer_.push(msg);
}

void DummyTransport::print(std::ostream& os) const {
    os << "DummyTransport[" << title_ << "]";
}

}  // namespace sandbox
}  // namespace server
}  // namespace multio
