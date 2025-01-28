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

    ret = multio_grib2_dict_create(&dictionary_01, "sh");

    printf("Set from c API (test03 reduced-gg keys) \n");


    ret = multio_grib2_dict_has(dictionary_01, "pentagonal-resolution-j", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "pentagonal-resolution-j", "1");
    ret = multio_grib2_dict_has(dictionary_01, "pentagonal-resolution-j", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "pentagonal-resolution-j", &readVal);
    printf("pentagonal-resolution-j: %s, %d, %d, %d\n", readVal, has1, has2, ret);
    assert(strcmp(readVal, "1") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "pentagonal-resolution-k", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "pentagonal-resolution-k", "2");
    ret = multio_grib2_dict_has(dictionary_01, "pentagonal-resolution-k", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "pentagonal-resolution-k", &readVal);
    printf("pentagonal-resolution-k': %s, %d, %d, %d\n", readVal, has1, has2, ret);
    assert(strcmp(readVal, "2") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "pentagonal-resolution-m", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "pentagonal-resolution-m", "3");
    ret = multio_grib2_dict_has(dictionary_01, "pentagonal-resolution-m", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "pentagonal-resolution-m", &readVal);
    printf("pentagonal-resolution-m: %s, %d, %d, %d\n", readVal, has1, has2, ret);
    assert(strcmp(readVal, "3") == 0);
    free(readVal);
    readVal = NULL;


    printf(" + iterate through the mars dictionary\n");
    void* iterator = NULL;
    char* key = NULL;
    char* value = NULL;
    ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    for (int i = 0; i < 2; ++i) {
        printf("Iterate: \"%s\": \"%s\"\n", key, value);
        free(key);
        free(value);
        ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    };

    printf("Destroy iterator from c API (test01) %p, %p \n", iterator, dictionary_01);
    ret = multio_grib2_dict_destroy_iterator(dictionary_01, &iterator);


    printf("Destroy from c API (test01) %p, %p \n", iterator, dictionary_01);

    ret = multio_grib2_dict_destroy(&dictionary_01);

    printf("Goodbye from c API (test03) %p, %p\n", iterator, dictionary_01);

    return 0;
}
