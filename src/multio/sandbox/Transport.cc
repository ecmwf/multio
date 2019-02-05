
#include "Transport.h"

namespace multio {
namespace sandbox {

Transport::Transport(const eckit::Configuration& config) :
    name_(get_config_value<std::string>(config, "name")) {}

Transport::~Transport() = default;

}  // namespace sandbox
}  // namespace multio
