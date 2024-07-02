#include "MetadataException.h"

namespace multio::message {

//-----------------------------------------------------------------------------

MetadataException::MetadataException(const std::string& reason, const eckit::CodeLocation& l) :
    eckit::Exception(std::string("MetadataException: ") + reason, l){};

MetadataKeyException::MetadataKeyException(const std::string& key, const std::string& more,
                                           const eckit::CodeLocation& l) :
    MetadataException(std::string("Exception for key \"") + key + std::string("\": ") + more, l){};

MetadataMissingKeyException::MetadataMissingKeyException(const std::string& missingKey, const eckit::CodeLocation& l) :
    MetadataKeyException(missingKey, std::string("missing"), l){};

MetadataWrongTypeException::MetadataWrongTypeException(const std::string& key, const eckit::CodeLocation& l) :
    MetadataException(std::string("differet key type contained for \"") + key + std::string("\""), l){};

MetadataWrongTypeException::MetadataWrongTypeException(std::size_t requestedIndex, std::size_t containedIndex,
                                                       const eckit::CodeLocation& l) :
    MetadataException(std::string("differet key type contained. Requested index: ") + std::to_string(requestedIndex)
                          + std::string(" contained index: ") + std::to_string(containedIndex),
                      l){};

MetadataWrongTypeException::MetadataWrongTypeException(const eckit::CodeLocation& l) :
    MetadataException(std::string("differet key type contained"), l){};

//-----------------------------------------------------------------------------

}  // namespace multio::message
