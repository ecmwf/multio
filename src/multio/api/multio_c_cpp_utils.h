#pragma once

#include "multio/message/Metadata.h"
#include "multio_c.h"

using multio::message::Metadata;

Metadata* multio_from_c(multio_metadata_t* multio);
multio_metadata_t* multio_to_c(Metadata* multio);

inline int multio_write_field(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size) {
    return (multio_write_float_field(mio, md, data, size));
};
inline int multio_write_field(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return (multio_write_double_field(mio, md, data, size));
};

inline int multio_write_mask(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size) {
    return (multio_write_float_mask(mio, md, data, size));
};
inline int multio_write_mask(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return (multio_write_double_mask(mio, md, data, size));
};
