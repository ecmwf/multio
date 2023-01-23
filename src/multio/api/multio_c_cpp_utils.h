#ifndef multio_api_multio_c_cpp_utils_H
#define multio_api_multio_c_cpp_utils_H

#include "multio_c.h"
#include "multio/message/Metadata.h"

using multio::message::Metadata;

Metadata* multio_from_c(multio_metadata_t* multio);
multio_metadata_t* multio_to_c(Metadata* multio);

#endif
