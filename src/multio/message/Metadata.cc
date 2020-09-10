
#include "Metadata.h"

#include <sstream>

#include "eckit/log/JSON.h"

namespace multio {
namespace message {

std::string to_string(const Metadata& metadata) {
    std::stringstream ss;
    eckit::JSON json(ss);
    json << metadata;

    return ss.str();
}

}  // namespace message
}  // namespace multio
