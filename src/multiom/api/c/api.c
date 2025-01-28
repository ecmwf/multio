#include "api.h"
#include <string.h>

// prototypes exposed by the fortran layer
int multio_grib2_dict_create_f(void** dict, char* dict_type, int len);
int multio_grib2_dict_set_f(void* dict, const char* key, int klen, const char* value, int vlen);
int multio_grib2_dict_get_f(void* dict, const char* key, int klen, char** value);
int multio_grib2_dict_has_f(void* dict, const char* key, int klen, int* has);


int multio_grib2_dict_create(void** dict, char* dict_type) {

    int len = strlen(dict_type);
    return multio_grib2_dict_create_f(dict, dict_type, len);
};


int multio_grib2_dict_set(void* dict, const char* key, const char* value) {

    int klen = strlen(key);
    int vlen = strlen(value);
    return multio_grib2_dict_set_f(dict, key, klen, value, vlen);
};


int multio_grib2_dict_get(void* dict, const char* key, char** value) {

    int klen = strlen(key);
    return multio_grib2_dict_get_f(dict, key, klen, value);
};


int multio_grib2_dict_has(void* dict, const char* key, int* has) {

    int klen = strlen(key);
    return multio_grib2_dict_has_f(dict, key, klen, has);
};