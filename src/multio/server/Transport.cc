
#include "Transport.h"

namespace multio {
namespace server {

Transport::Transport(const std::string& title, const size_t no_serv) :
    title_(title),
    no_servers_(no_serv) {}

Transport::~Transport() = default;

const std::string& Transport::title() const {
    return title_;
}

size_t Transport::no_servers() const {
    return no_servers_;
}

size_t Transport::no_clients() const {
    return size() - no_servers();
}

}  // namespace server
}  // namespace multio
