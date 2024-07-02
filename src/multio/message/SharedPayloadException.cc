#include "SharedPayloadException.h"

namespace multio::message {

//-----------------------------------------------------------------------------

SharedPayloadException::SharedPayloadException(const std::string& reason, const eckit::CodeLocation& l) :
    eckit::Exception(std::string("SharedPayloadException: ") + reason, l) {};

PayloadNotWritableException::PayloadNotWritableException(const eckit::CodeLocation& l) :
    SharedPayloadException(
        std::string("Payload is referenced not writable - may need to acquire before trying to write to it."), l) {};


//-----------------------------------------------------------------------------

}  // namespace multio::message
