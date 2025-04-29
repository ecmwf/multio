#include "api.h"
#include <cstring>
#include <string>

extern "C" {

// prototypes exposed by the fortran layer
int multio_grib2_dict_create_f(void** dict, char* dict_type, int len);
int multio_grib2_dict_set_f(void* dict, const char* key, int klen, const char* value, int vlen);
int multio_grib2_dict_set_int64_f(void* dict, const char* key, int klen, int64_t value );
int multio_grib2_dict_set_real64_f(void* dict, const char* key, int klen, double value );
int multio_grib2_dict_set_int64_array_f(void* dict, const char* key, int klen, const int64_t* value, int vlen);
int multio_grib2_dict_set_real64_array_f(void* dict, const char* key, int klen, const double* value, int vlen);
int multio_grib2_dict_get_f(void* dict, const char* key, int klen, char** value);
int multio_grib2_dict_has_f(void* dict, const char* key, int klen, int* has);
int multio_grib2_dict_to_yaml_f(void* dict, const char* fname, int len);
int multio_grib2_dict_to_json_f(void* dict, char** value);


int multio_grib2_dict_create(void** dict, char* dict_type) {
    int len = std::strlen(dict_type);
    return multio_grib2_dict_create_f(dict, dict_type, len);
};


int multio_grib2_dict_set(void* dict, const char* key, const char* value) {
    if (std::strcmp(key, "param") == 0) {
        std::string newVal;  // = std::to_string(metkit::Param(value).paramId());

        int klen = std::strlen(key);
        int vlen = std::strlen(newVal.c_str());
        return multio_grib2_dict_set_f(dict, key, klen, newVal.c_str(), vlen);
    }
    else {
        int klen = std::strlen(key);
        int vlen = std::strlen(value);
        return multio_grib2_dict_set_f(dict, key, klen, value, vlen);
    }
};

int multio_grib2_dict_set_int64(void* dict, const char* key, int64_t value) {
    int klen = std::strlen(key);
    return multio_grib2_dict_set_int64_f(dict, key, klen, value);
};

int multio_grib2_dict_set_double(void* dict, const char* key, double value) {
    int klen = std::strlen(key);
    return multio_grib2_dict_set_real64_f(dict, key, klen, value);
};


int multio_grib2_dict_set_int64_array(void* dict, const char* key, const int64_t* value, int vlen) {
    int klen = std::strlen(key);
    return multio_grib2_dict_set_int64_array_f(dict, key, klen, value, vlen);
};

int multio_grib2_dict_set_double_array(void* dict, const char* key, const double* value, int vlen) {
    int klen = std::strlen(key);
    return multio_grib2_dict_set_real64_array_f(dict, key, klen, value, vlen);
};


int multio_grib2_dict_get(void* dict, const char* key, char** value) {

    int klen = std::strlen(key);
    return multio_grib2_dict_get_f(dict, key, klen, value);
};


int multio_grib2_dict_has(void* dict, const char* key, int* has) {

    int klen = std::strlen(key);
    return multio_grib2_dict_has_f(dict, key, klen, has);
};


int multio_grib2_dict_to_yaml(void* dict, const char* fname) {
    int len = std::strlen(fname);
    return multio_grib2_dict_to_yaml_f(dict, fname, len);
};

int multio_grib2_dict_to_json(void* dict, char** value) {
    return multio_grib2_dict_to_json_f(dict, value);
};
}
