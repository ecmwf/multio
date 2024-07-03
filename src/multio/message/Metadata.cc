
#include "Metadata.h"

#include <sstream>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/JSON.h"

namespace multio::message {

Metadata::Metadata(const eckit::Configuration& config) : eckit::LocalConfiguration{config} {}

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

MetadataException::MetadataException(const std::string& reason, const eckit::CodeLocation& l) :
    eckit::Exception(std::string("MetadataException: ") + reason, l) {};

MetadataMissingKeyException::MetadataMissingKeyException(const std::string& missingKey, const eckit::CodeLocation& l) :
    MetadataException(std::string("Missing key \"") + missingKey + std::string("\""), l) {};


}  // namespace multio::message
