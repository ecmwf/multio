#pragma once
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned long size_t;

// multio_grib2 dictionary
int multio_grib2_dict_create(void** dict, char* dict_type);
int multio_grib2_dict_get(void* dict, const char* key, char** value);
int multio_grib2_dict_has(void* dict, const char* key, int* has);
int multio_grib2_dict_iterate(void* dict, void** iterator, char** key, char** value);
int multio_grib2_dict_destroy_iterator(void* dict, void** iterator);
int multio_grib2_dict_destroy(void** dict);

// multio_grib2 dictionary string setter
// This setter is generic and converts to the underlying type
// Numbers can be but as string represention, e.g. "123" for an integer or "123.456" for a double
// Arrays can be passed as json arrays, e.g. [1,2,3] or [1.1, 2.2, 3.3] for integer & double arrays (e.g. pl or pv)
int multio_grib2_dict_set(void* dict, const char* key, const char* value);

// multio_grib2 dictionary typed setters -
// These specialized calls won't perform any conversion and must match the underlying type for a key.
int multio_grib2_dict_set_int64(void* dict, const char* key, int64_t value);
int multio_grib2_dict_set_double(void* dict, const char* key, double value);
int multio_grib2_dict_set_int64_array(void* dict, const char* key, const int64_t* value, int vlen);
int multio_grib2_dict_set_double_array(void* dict, const char* key, const double* value, int vlen);

int multio_grib2_dict_to_yaml(void* dict, const char* fname);
int multio_grib2_dict_to_json(void* dict, char** value);

int multio_grib2_dict_set_geometry(void* dict, const void* geom_dict);

// Initialize all the options to a relevant default
int multio_grib2_init_options(void** opt_dict);

// options dict can be null, in this case default options can be applied
int multio_grib2_encoder_open(void* opt_dict, void** multio_grib2);
int multio_grib2_encoder_extract_metadata(void* multio_grib2, void* grib, void** mars_dict, void** par_dict);
int multio_grib2_encoder_encode64(void* multio_grib2, void* mars_dict, void* par_dict, const double* data,
                                  size_t data_len, void** out_handle);
int multio_grib2_encoder_encode32(void* multio_grib2, void* mars_dict, void* par_dict, const float* data,
                                  size_t data_len, void** out_handle);
int multio_grib2_encoder_close(void** multio_grib2);


#ifdef __cplusplus
} /* extern "C" */
#endif
