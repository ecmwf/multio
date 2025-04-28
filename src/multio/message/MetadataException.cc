#include "MetadataException.h"

namespace multio::message {

//-----------------------------------------------------------------------------

MetadataException::MetadataException(const std::string& reason, const eckit::CodeLocation& l) :
    eckit::Exception(std::string("MetadataException: ") + reason, l) {};

MetadataKeyException::MetadataKeyException(const std::string& key, const std::string& more,
                                           const eckit::CodeLocation& l) :
    MetadataException(std::string("Exception for key \"") + key + std::string("\": ") + more, l) {};

MetadataMissingKeyException::MetadataMissingKeyException(const std::string& missingKey, const eckit::CodeLocation& l) :
    MetadataKeyException(missingKey, std::string("missing"), l) {};

MetadataWrongTypeException::MetadataWrongTypeException(const std::string& key, const eckit::CodeLocation& l) :
    MetadataException(std::string("different key type contained for \"") + key + std::string("\""), l) {};

MetadataWrongTypeException::MetadataWrongTypeException(const std::string& requestedType,
                                                       const std::string& containedType, const eckit::CodeLocation& l) :
    MetadataException(std::string("different key type contained. Requested type: ") + requestedType
                          + std::string(" contained type: ") + containedType,
                      l) {};

MetadataWrongTypeException::MetadataWrongTypeException(const eckit::CodeLocation& l) :
    MetadataException(std::string("different key type contained"), l) {};

//-----------------------------------------------------------------------------

}  // namespace multio::message
