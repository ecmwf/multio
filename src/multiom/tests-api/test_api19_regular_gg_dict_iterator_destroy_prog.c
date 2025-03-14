#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "api.h"

int main() {

    int ret;
    int has1;
    int has2;
    void* dictionary_01 = NULL;
    char* readVal = NULL;

    printf("Welcome from c API (test03) %p\n", dictionary_01);

    ret = multio_grib2_dict_create(&dictionary_01, "regular-gg");

    printf("Set from c API (test03 regular-gg keys) \n");


    ret = multio_grib2_dict_has(dictionary_01, "truncate-degrees", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "truncate-degrees", "1");
    ret = multio_grib2_dict_has(dictionary_01, "truncate-degrees", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "truncate-degrees", &readVal);
    // printf("truncate-degrees: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "1") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "number-of-points-along-a-meridian", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "number-of-points-along-a-meridian", "2");
    ret = multio_grib2_dict_has(dictionary_01, "number-of-points-along-a-meridian", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "number-of-points-along-a-meridian", &readVal);
    // printf("number-of-points-along-a-meridian': %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "2") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "number-of-parallels-between-pole-and-equator", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "number-of-parallels-between-pole-and-equator", "3");
    ret = multio_grib2_dict_has(dictionary_01, "number-of-parallels-between-pole-and-equator", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "number-of-parallels-between-pole-and-equator", &readVal);
    // printf("number-of-parallels-between-pole-and-equator: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "3") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "latitude-of-first-grid-point-in-degrees", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "latitude-of-first-grid-point-in-degrees", "1.2");
    ret = multio_grib2_dict_has(dictionary_01, "latitude-of-first-grid-point-in-degrees", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "latitude-of-first-grid-point-in-degrees", &readVal);
    // printf("latitude-of-first-grid-point-in-degrees: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "1.200000e+00") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "longitude-of-first-grid-point-in-degrees",
                                &has1);  // TODO find a valid offset
    ret = multio_grib2_dict_set(dictionary_01, "longitude-of-first-grid-point-in-degrees",
                                "2.3");  // TODO find a valid offset
    ret = multio_grib2_dict_has(dictionary_01, "longitude-of-first-grid-point-in-degrees",
                                &has2);  // TODO find a valid offset
    ret = multio_grib2_dict_get(dictionary_01, "longitude-of-first-grid-point-in-degrees",
                                &readVal);  // TODO find a valid offset
    // printf("longitude-of-first-grid-point-in-degrees: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "2.300000e+00") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "latitude-of-last-grid-point-in-degrees", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "latitude-of-last-grid-point-in-degrees", "3.4");
    ret = multio_grib2_dict_has(dictionary_01, "latitude-of-last-grid-point-in-degrees", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "latitude-of-last-grid-point-in-degrees", &readVal);
    // printf("latitude-of-last-grid-point-in-degrees: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "3.400000e+00") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "longitude-of-last-grid-point-in-degrees", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "longitude-of-last-grid-point-in-degrees", "4.5");
    ret = multio_grib2_dict_has(dictionary_01, "longitude-of-last-grid-point-in-degrees", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "longitude-of-last-grid-point-in-degrees", &readVal);
    // printf("longitude-of-last-grid-point-in-degrees: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "4.500000e+00") == 0);
    free(readVal);
    readVal = NULL;

    printf(" + iterate through the mars dictionary\n");
    void* iterator = NULL;
    char* key = NULL;
    char* value = NULL;
    ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    for (int i = 0; i < 3; ++i) {
        printf("Iterate: \"%s\": \"%s\"\n", key, value);
        free(key);
        free(value);
        ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    };


    printf("Destroy iterator from c API (test01) %p, %p \n", iterator, dictionary_01);
    ret = multio_grib2_dict_destroy_iterator(dictionary_01, &iterator);

    printf("Destroy from c API (test01) %p, %p \n", iterator, dictionary_01);

    ret = multio_grib2_dict_destroy(&dictionary_01);

    // Test that the destroyer reset also to NULL
    printf("Goodbye from c API (test03) %p, %p \n", iterator, dictionary_01);

    return 0;
}
