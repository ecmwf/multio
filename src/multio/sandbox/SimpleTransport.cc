
#include <iostream>

#include "SimpleTransport.h"

namespace multio {
namespace sandbox {

SimpleTransport::SimpleTransport(const eckit::Configuration& config) : Transport{config} {}
SimpleTransport::~SimpleTransport() = default;

void SimpleTransport::receive(Message& msg) {
    msg = buffer_.pop();
}

void SimpleTransport::send(const Message& msg) {
    buffer_.push(msg);
}

void SimpleTransport::print(std::ostream& os) const {
    os << "SimpleTransport(name = " << name_ << ")";
}

}  // namespace sandbox
}  // namespace multio
