
#include "Transport.h"

namespace multio {
namespace attic {

Transport::Transport(const std::string& title, const size_t no_serv) :
    title_(title),
    noServers_(no_serv) {}

Transport::~Transport() = default;

const std::string& Transport::title() const {
    return title_;
}

size_t Transport::noServers() const {
    return noServers_;
}

size_t Transport::noClients() const {
    return size() - noServers();
}

}  // namespace attic
}  // namespace multio
