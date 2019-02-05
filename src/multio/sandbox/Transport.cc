
#include "Transport.h"

namespace multio {
namespace sandbox {

namespace {
}  // namespace

Transport::Transport(const eckit::LocalConfiguration& config) : config_(config) {}

Transport::~Transport() = default;

void Transport::print(std::ostream& os) const {
    os << "Transport[" << get_config_value<std::string>(config_, "name") << "]";
}

}  // namespace sandbox
}  // namespace multio
