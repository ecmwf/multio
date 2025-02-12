#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "api.h"

int main() {

    int ret;
    void* dictionary_01 = NULL;
    char* readVal = NULL;

    void* iterator = NULL;
    char* key = NULL;
    char* value = NULL;

    printf("Welcome from c API (test04) %p\n", dictionary_01);

    ret = multio_grib2_dict_create(&dictionary_01, "mars");

    // Set stream
    int has1 = -99999;
    int has2 = -99999;
    // ret = multio_grib2_dict_has( dictionary_01, "stream", &has1 );
    ret = multio_grib2_dict_set(dictionary_01, "stream", "oper");
    ret = multio_grib2_dict_set(dictionary_01, "type", "fc");
    ret = multio_grib2_dict_set(dictionary_01, "class", "rd");
    ret = multio_grib2_dict_set(dictionary_01, "origin", "ecmf");
    ret = multio_grib2_dict_set(dictionary_01, "packing", "ccsds");
    // ret = multio_grib2_dict_get( dictionary_01, "stream", &readVal );
    // ret = multio_grib2_dict_has( dictionary_01, "stream", &has2 );
    // printf("stream: %s, %d, %d\n", readVal, has1, has2);
    // assert(strcmp(readVal, "oper") == 0);
    // free(readVal);

    printf("iterate through the dictionary\n");
    ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    while (iterator != NULL) {
        printf("Iterate: \"%s\": \"%s\"\n", key, value);
        free(key);
        free(value);
        ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    };

    /*
    printf( "Test iterator construction c API (test04) %p\n", dictionary_01 );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    ret = multio_grib2_dict_iterate( dictionary_01, &iterator, &key, &value );
    */

    ret = multio_grib2_dict_destroy(&dictionary_01);


    printf("Goodbye from c API (test04) %p\n", dictionary_01);

    return 0;
}
