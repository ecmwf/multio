#pragma once

#include "multio/message/Metadata.h"

#include "multio_c.h"

using multio::message::Metadata;

Metadata* multio_from_c(multio_metadata_t* multio);
multio_metadata_t* multio_to_c(Metadata* multio);


// overloaded field write
inline int multio_write_field(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size) {
    return (multio_write_field_float(mio, md, data, size));
};
inline int multio_write_field(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return (multio_write_field_double(mio, md, data, size));
};


// overloaded mask write
inline int multio_write_mask(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size) {
    return (multio_write_mask_float(mio, md, data, size));
};
inline int multio_write_mask(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return (multio_write_mask_double(mio, md, data, size));
};


// overloaded metadata set
inline int multio_metadata_set(multio_metadata_t* md, const char* key, int value) {
    return (multio_metadata_set_int(md, key, value));
};
inline int multio_metadata_set(multio_metadata_t* md, const char* key, long value) {
    return (multio_metadata_set_int(md, key, value));
};
inline int multio_metadata_set(multio_metadata_t* md, const char* key, long long value) {
    return (multio_metadata_set_int(md, key, value));
};
inline int multio_metadata_set(multio_metadata_t* md, const char* key, const char* value) {
    return (multio_metadata_set_string(md, key, value));
};
inline int multio_metadata_set(multio_metadata_t* md, const char* key, bool value) {
    return (multio_metadata_set_bool(md, key, value));
};
inline int multio_metadata_set(multio_metadata_t* md, const char* key, float value) {
    return (multio_metadata_set_double(md, key, value));
};
inline int multio_metadata_set(multio_metadata_t* md, const char* key, double value) {
    return (multio_metadata_set_double(md, key, value));
};
