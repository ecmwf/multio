#include "DataModellingException.h"

namespace multio::datamod {

//-----------------------------------------------------------------------------

DataModellingException::DataModellingException(const std::string& reason, const eckit::CodeLocation& l) :
    eckit::Exception(std::string("DataModellingException: ") + reason, l) {};

//-----------------------------------------------------------------------------

}  // namespace multio::message
