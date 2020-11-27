
#include "Metadata.h"

#include <sstream>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/JSON.h"

namespace multio {
namespace message {

std::string to_string(const Metadata& metadata) {
    std::stringstream ss;
    eckit::JSON json(ss);
    json << metadata;

    return ss.str();
}

Metadata to_metadata(const std::string& fieldId) {
    const eckit::Configuration& config{eckit::YAMLConfiguration{fieldId}};
    return Metadata{config};
}

}  // namespace message
}  // namespace multio
