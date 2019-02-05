
#include <iostream>

#include "SimpleTransport.h"

namespace multio {
namespace sandbox {

SimpleTransport::SimpleTransport(const eckit::LocalConfiguration& config) : Transport{config} {}
SimpleTransport::~SimpleTransport() = default;

void SimpleTransport::receive(Message& msg) {
    msg = internalBuffer_.pop();
}

void SimpleTransport::send(const Message& msg) {
    internalBuffer_.push(msg);
}

}  // namespace sandbox
}  // namespace multio
