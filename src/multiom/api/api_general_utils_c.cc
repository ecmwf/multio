
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iomanip>
#include <iostream>
#include "eccodes.h"

extern "C" {

int convert_int8_to_cstring(const void* intvar, char** cstring) {
    if (intvar == NULL) {
        return 1;
    }
    int8_t* val = (int8_t*)intvar;
    const int max_length = snprintf(NULL, 0, "%" PRId8, *val) + 1;
    *cstring = (char*)malloc(max_length * sizeof(char));
    if (*cstring == NULL) {
        return 1;
    }
    memset(*cstring, 0, max_length * sizeof(char));
    sprintf(*cstring, "%" PRId8, *val);
    return 0;
};

int convert_int16_to_cstring(const void* intvar, char** cstring) {
    if (intvar == NULL) {
        return 1;
    }
    int16_t* val = (int16_t*)intvar;
    const int max_length = snprintf(NULL, 0, "%" PRId16, *val) + 1;
    *cstring = (char*)malloc(max_length * sizeof(char));
    if (*cstring == NULL) {
        return 1;
    }
    memset(*cstring, 0, max_length * sizeof(char));
    sprintf(*cstring, "%" PRId16, *val);
    return 0;
};

int convert_int32_to_cstring(const void* intvar, char** cstring) {
    if (intvar == NULL) {
        return 1;
    }
    int32_t* val = (int32_t*)intvar;
    const int max_length = snprintf(NULL, 0, "%" PRId32, *val) + 1;
    *cstring = (char*)malloc(max_length * sizeof(char));
    if (*cstring == NULL) {
        return 1;
    }
    memset(*cstring, 0, max_length * sizeof(char));
    sprintf(*cstring, "%" PRId32, *val);
    return 0;
};

int convert_int64_to_cstring(const void* intvar, char** cstring) {
    if (intvar == NULL) {
        return 1;
    }
    int64_t* val = (int64_t*)intvar;
    const int max_length = snprintf(NULL, 0, "%" PRId64, *val) + 1;
    *cstring = (char*)malloc(max_length * sizeof(char));
    if (*cstring == NULL) {
        return 1;
    }
    memset(*cstring, 0, max_length * sizeof(char));
    sprintf(*cstring, "%" PRId64, *val);
    return 0;
};

int convert_real32_to_cstring(const void* floatvar, char** cstring) {
    if (floatvar == NULL) {
        return 1;
    }
    float* val = (float*)floatvar;
    const int max_length = snprintf(NULL, 0, "%e", *val) + 1;
    *cstring = (char*)malloc(max_length * sizeof(char));
    if (*cstring == NULL) {
        return 1;
    }
    memset(*cstring, 0, max_length * sizeof(char));
    sprintf(*cstring, "%e", *val);
    return 0;
};

int convert_real64_to_cstring(const void* floatvar, char** cstring) {
    if (floatvar == NULL) {
        return 1;
    }
    double* val = (double*)floatvar;
    const int max_length = snprintf(NULL, 0, "%le", *val) + 1;
    *cstring = (char*)malloc(max_length * sizeof(char));
    if (*cstring == NULL) {
        return 1;
    }
    memset(*cstring, 0, max_length * sizeof(char));
    sprintf(*cstring, "%le", *val);
    return 0;
};

int convert_fstring_to_cstring(const char* fstring, char** cstring) {
    if (fstring == NULL) {
        return 1;
    }
    int len = strlen(fstring);
    *cstring = (char*)malloc(len + 1);
    if (*cstring == NULL) {
        return 1;
    }
    memset(*cstring, 0, len + 1);
    strcpy(*cstring, fstring);
    return 0;
};


int convert_fstring_array_to_cstring_array(const char* values, int n, int m, char*** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0 || m < 0) {
        return 1;
    }
    *str_array = (char**)malloc(n * sizeof(char**));
    if (*str_array == NULL) {
        return 1;
    }
    int lo = 0;
    int hi = 0;
    int cnt = 0;
    for (int i = 0; i < m; i++) {
        if (values[i] == '\0') {
            hi = i;
            size_t sz = hi - lo + 1;
            char* tmp = (char*)malloc(sz * sizeof(char));
            memset(tmp, 0, sz * sizeof(char));
            strncpy(tmp, &values[lo], sz);
            (*str_array)[cnt] = tmp;
            lo = i + 1;
            cnt++;
        }
    }
    return 0;
};


int convert_int8_array_to_cstring_array(const void* values, int n, char*** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    *str_array = (char**)malloc(n * sizeof(char**));
    if (*str_array == NULL) {
        return 1;
    }
    int8_t* val = (int8_t*)values;
    for (int i = 0; i < n; i++) {
        const int max_length = snprintf(NULL, 0, "%" PRId8, val[i]) + 1;
        char* tmp = (char*)malloc(max_length * sizeof(char));
        memset(tmp, 0, max_length * sizeof(char));
        sprintf(tmp, "%" PRId8, val[i]);
        (*str_array)[i] = tmp;
    }
    return 0;
};


int convert_int16_array_to_cstring_array(const void* values, int n, char*** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    *str_array = (char**)malloc(n * sizeof(char**));
    if (*str_array == NULL) {
        return 1;
    }
    int16_t* val = (int16_t*)values;
    for (int i = 0; i < n; i++) {
        const int max_length = snprintf(NULL, 0, "%" PRId16, val[i]) + 1;
        char* tmp = (char*)malloc(max_length * sizeof(char));
        memset(tmp, 0, max_length * sizeof(char));
        sprintf(tmp, "%" PRId16, val[i]);
        (*str_array)[i] = tmp;
    }
    return 0;
};


int convert_int32_array_to_cstring_array(const void* values, int n, char*** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    *str_array = (char**)malloc(n * sizeof(char**));
    if (*str_array == NULL) {
        return 1;
    }
    int32_t* val = (int32_t*)values;
    for (int i = 0; i < n; i++) {
        const int max_length = snprintf(NULL, 0, "%" PRId32, val[i]) + 1;
        char* tmp = (char*)malloc(max_length * sizeof(char));
        memset(tmp, 0, max_length * sizeof(char));
        sprintf(tmp, "%" PRId32, val[i]);
        (*str_array)[i] = tmp;
    }
    return 0;
};


int convert_int64_array_to_cstring_array(const void* values, int n, char*** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    *str_array = (char**)malloc(n * sizeof(char**));
    if (*str_array == NULL) {
        return 1;
    }
    int64_t* val = (int64_t*)values;
    for (int i = 0; i < n; i++) {
        const int max_length = snprintf(NULL, 0, "%" PRId64, val[i]) + 1;
        char* tmp = (char*)malloc(max_length * sizeof(char));
        memset(tmp, 0, max_length * sizeof(char));
        sprintf(tmp, "%" PRId64, val[i]);
        (*str_array)[i] = tmp;
    }
    return 0;
};

int convert_real32_array_to_cstring_array(const void* values, int n, char*** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    *str_array = (char**)malloc(n * sizeof(char**));
    if (*str_array == NULL) {
        return 1;
    }
    float* val = (float*)values;
    for (int i = 0; i < n; i++) {
        const int max_length = snprintf(NULL, 0, "%e", val[i]) + 1;
        char* tmp = (char*)malloc(max_length * sizeof(char));
        memset(tmp, 0, max_length * sizeof(char));
        sprintf(tmp, "%e", val[i]);
        (*str_array)[i] = tmp;
    }
    return 0;
};

int convert_real64_array_to_cstring_array(const void* values, int n, char*** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    *str_array = (char**)malloc(n * sizeof(char**));
    if (*str_array == NULL) {
        return 1;
    }
    double* val = (double*)values;
    for (int i = 0; i < n; i++) {
        const int max_length = snprintf(NULL, 0, "%le", val[i]) + 1;
        char* tmp = (char*)malloc(max_length * sizeof(char));
        memset(tmp, 0, max_length * sizeof(char));
        sprintf(tmp, "%le", val[i]);
        (*str_array)[i] = tmp;
    }
    return 0;
};


int convert_int8_array_to_cstring(const void* values, int n, char** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    int len = 3;  // "[ " and the null terminator
    int8_t* val = (int8_t*)values;
    for (int i = 0; i < n; i++) {
        len += snprintf(NULL, 0, "%" PRId8, val[i]) + 2;  // ", " and " ]"
    }
    *str_array = (char*)malloc(len * sizeof(char));
    if (*str_array == NULL) {
        return 1;
    }
    memset(*str_array, 0, len * sizeof(char));
    char* ptr = *str_array;
    ptr += sprintf(ptr, "[ ");
    for (int i = 0; i < n; i++) {
        ptr += sprintf(ptr, "%" PRId8, val[i]);
        if (i < n - 1) {
            ptr += sprintf(ptr, ", ");
        }
        else {
            ptr += sprintf(ptr, " ]");
        }
    }
    return 0;
};


int convert_int16_array_to_cstring(const void* values, int n, char** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    int len = 3;  // "[ " and the null terminator
    int16_t* val = (int16_t*)values;
    for (int i = 0; i < n; i++) {
        len += snprintf(NULL, 0, "%" PRId16, val[i]) + 2;  // ", " and " ]"
    }
    *str_array = (char*)malloc(len * sizeof(char));
    if (*str_array == NULL) {
        return 1;
    }
    memset(*str_array, 0, len * sizeof(char));
    char* ptr = *str_array;
    ptr += sprintf(ptr, "[ ");
    for (int i = 0; i < n; i++) {
        ptr += sprintf(ptr, "%" PRId16, val[i]);
        if (i < n - 1) {
            ptr += sprintf(ptr, ", ");
        }
        else {
            ptr += sprintf(ptr, " ]");
        }
    }
    return 0;
};


int convert_int32_array_to_cstring(const void* values, int n, char** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    int len = 3;  // "[ " and the null terminator
    int32_t* val = (int32_t*)values;
    for (int i = 0; i < n; i++) {
        len += snprintf(NULL, 0, "%" PRId32, val[i]) + 2;  // ", " and " ]"
    }
    *str_array = (char*)malloc(len * sizeof(char));
    if (*str_array == NULL) {
        return 1;
    }
    memset(*str_array, 0, len * sizeof(char));
    char* ptr = *str_array;
    ptr += sprintf(ptr, "[ ");
    for (int i = 0; i < n; i++) {
        ptr += sprintf(ptr, "%" PRId32, val[i]);
        if (i < n - 1) {
            ptr += sprintf(ptr, ", ");
        }
        else {
            ptr += sprintf(ptr, " ]");
        }
    }
    return 0;
};


int convert_int64_array_to_cstring(const void* values, int n, char** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    int len = 3;  // "[ " and the null terminator
    int64_t* val = (int64_t*)values;
    for (int i = 0; i < n; i++) {
        len += snprintf(NULL, 0, "%" PRId64, val[i]) + 2;  // ", " and " ]"
    }
    *str_array = (char*)malloc(len * sizeof(char));
    if (*str_array == NULL) {
        return 1;
    }
    memset(*str_array, 0, len * sizeof(char));
    char* ptr = *str_array;
    ptr += sprintf(ptr, "[ ");
    for (int i = 0; i < n; i++) {
        ptr += sprintf(ptr, "%" PRId64, val[i]);
        if (i < n - 1) {
            ptr += sprintf(ptr, ", ");
        }
        else {
            ptr += sprintf(ptr, " ]");
        }
    }
    return 0;
};


int convert_real32_array_to_cstring(const void* values, int n, char** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    int len = 3;  // "[ " and the null terminator
    float* val = (float*)values;
    for (int i = 0; i < n; i++) {
        len += snprintf(NULL, 0, "%e", val[i]) + 2;  // ", " and " ]"
    }
    *str_array = (char*)malloc(len * sizeof(char));
    if (*str_array == NULL) {
        return 1;
    }
    memset(*str_array, 0, len * sizeof(char));
    char* ptr = *str_array;
    ptr += sprintf(ptr, "[ ");
    for (int i = 0; i < n; i++) {
        ptr += sprintf(ptr, "%e", val[i]);
        if (i < n - 1) {
            ptr += sprintf(ptr, ", ");
        }
        else {
            ptr += sprintf(ptr, " ]");
        }
    }
    return 0;
};


int convert_real64_array_to_cstring(const void* values, int n, char** str_array) {
    if (values == NULL) {
        return 1;
    }
    if (n < 0) {
        return 1;
    }
    int len = 3;  // "[ " and the null terminator
    double* val = (double*)values;
    for (int i = 0; i < n; i++) {
        len += snprintf(NULL, 0, "%le", val[i]) + 2;  // ", " and " ]"
    }
    *str_array = (char*)malloc(len * sizeof(char));
    if (*str_array == NULL) {
        return 1;
    }
    memset(*str_array, 0, len * sizeof(char));
    char* ptr = *str_array;
    ptr += sprintf(ptr, "[ ");
    for (int i = 0; i < n; i++) {
        ptr += sprintf(ptr, "%le", val[i]);
        if (i < n - 1) {
            ptr += sprintf(ptr, ", ");
        }
        else {
            ptr += sprintf(ptr, " ]");
        }
    }
    return 0;
};


int allocate_iterator(void** iterator) {
    *iterator = (int*)malloc(sizeof(int*));
    return 0;
}


int free_iterator(void* iterator) {
    free(iterator);
    return 0;
}


int set_codes_handle_c(const void* values, int len, void** location) {
    if (values == NULL) {
        return 1;
    }
    if (len < 0) {
        return 1;
    }
    if (location == NULL) {
        return 1;
    }
    *location = codes_handle_new_from_message(NULL, values, len);
    if (*location == NULL) {
        return 1;
    }
    return 0;
};

int copy_f_buf_to_c_buf_c(const char* val, int len, void** location) {
    *location = malloc(len * sizeof(char));
    if (*location == NULL) {
        return -1;
    }
    memcpy(*location, val, len);
    return 0;
}
}
