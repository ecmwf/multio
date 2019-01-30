#include "Transport.h"

namespace multio {
namespace server {
namespace sandbox {

Transport::Transport(const std::string& title) : title_(title) {}

Transport::~Transport() = default;

}  // namespace sandbox
}  // namespace server
}  // namespace multio
