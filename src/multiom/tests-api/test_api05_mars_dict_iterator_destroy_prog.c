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

    ret = multio_grib2_dict_create(&dictionary_01, "mars");

    printf("Set from c API (test03 MARS keys) \n");


    ret = multio_grib2_dict_has(dictionary_01, "stream", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "stream", "oper");
    ret = multio_grib2_dict_has(dictionary_01, "stream", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "stream", &readVal);
    // printf("stream: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "oper") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "type", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "type", "fc");
    ret = multio_grib2_dict_has(dictionary_01, "type", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "type", &readVal);
    // printf("type: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "fc") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "class", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "class", "rd");
    ret = multio_grib2_dict_has(dictionary_01, "class", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "class", &readVal);
    // printf("class: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "rd") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "origin", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "origin", "ecmf");
    ret = multio_grib2_dict_has(dictionary_01, "origin", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "origin", &readVal);
    // printf("origin: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "ecmf") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "anoffset", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "anoffset", "1234");
    ret = multio_grib2_dict_has(dictionary_01, "anoffset", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "anoffset", &readVal);
    // printf("anoffset: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "1234") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "packing", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "packing", "ccsds");
    ret = multio_grib2_dict_has(dictionary_01, "packing", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "packing", &readVal);
    // printf("packing: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "ccsds") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "number", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "number", "123");
    ret = multio_grib2_dict_has(dictionary_01, "number", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "number", &readVal);
    // printf("number: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "123") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "ident", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "ident", "1");
    ret = multio_grib2_dict_has(dictionary_01, "ident", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "ident", &readVal);
    // printf("ident: %s, %d, %d, %d\n", readVal, has1, has2, ret );

    ret = multio_grib2_dict_has(dictionary_01, "instrument", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "instrument", "2");
    ret = multio_grib2_dict_has(dictionary_01, "instrument", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "instrument", &readVal);
    // printf("instrument: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "2") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "channel", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "channel", "3");
    ret = multio_grib2_dict_has(dictionary_01, "channel", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "channel", &readVal);
    // printf("channel: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "3") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "paramType", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "paramType", "optical");
    ret = multio_grib2_dict_has(dictionary_01, "paramType", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "paramType", &readVal);
    // printf("paramType: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "optical") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "chem", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "chem", "1");
    ret = multio_grib2_dict_has(dictionary_01, "chem", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "chem", &readVal);
    // printf("chem: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "1") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "param", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "param", "123");
    ret = multio_grib2_dict_has(dictionary_01, "param", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "param", &readVal);
    // printf("param: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "123") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "model", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "model", "IFS");
    ret = multio_grib2_dict_has(dictionary_01, "model", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "model", &readVal);
    // printf("model: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "IFS") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "levtype", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "levtype", "ml");
    ret = multio_grib2_dict_has(dictionary_01, "levtype", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "levtype", &readVal);
    // printf("levtype: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "ml") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "levelist", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "levelist", "10");
    ret = multio_grib2_dict_has(dictionary_01, "levelist", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "levelist", &readVal);
    // printf("levelist: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "10") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "direction", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "direction", "11");
    ret = multio_grib2_dict_has(dictionary_01, "direction", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "direction", &readVal);
    // printf("direction: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "11") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "frequency", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "frequency", "12");
    ret = multio_grib2_dict_has(dictionary_01, "frequency", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "frequency", &readVal);
    // printf("frequency: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "12") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "date", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "date", "20220812");
    ret = multio_grib2_dict_has(dictionary_01, "date", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "date", &readVal);
    // printf("date: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "20220812") == 0);
    free(readVal);
    readVal = NULL;
    
    ret = multio_grib2_dict_has(dictionary_01, "hdate", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "hdate", "20230913");
    ret = multio_grib2_dict_has(dictionary_01, "hdate", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "hdate", &readVal);
    // printf("hdate: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "20230913") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "time", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "time", "800");
    ret = multio_grib2_dict_has(dictionary_01, "time", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "time", &readVal);
    // printf("time: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "800") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "step", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "step", "8");
    ret = multio_grib2_dict_has(dictionary_01, "step", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "step", &readVal);
    // printf("step: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "8") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "repres", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "repres", "gg");
    ret = multio_grib2_dict_has(dictionary_01, "repres", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "repres", &readVal);
    // printf("repres: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "gg") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "truncation", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "truncation", "3");
    ret = multio_grib2_dict_has(dictionary_01, "truncation", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "truncation", &readVal);
    // printf("truncation: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "3") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "timeproc", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "timeproc", "5");
    ret = multio_grib2_dict_has(dictionary_01, "timeproc", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "timeproc", &readVal);
    // printf("timeproc: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "5") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "expver", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "expver", "ABCD");
    ret = multio_grib2_dict_has(dictionary_01, "expver", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "expver", &readVal);
    // printf("expver: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "ABCD") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "grid", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "grid", "O1280");
    ret = multio_grib2_dict_has(dictionary_01, "grid", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "grid", &readVal);
    // printf("grid: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "O1280") == 0);
    free(readVal);
    readVal = NULL;
    
    ret = multio_grib2_dict_has(dictionary_01, "wavelength", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "wavelength", "123-456");
    ret = multio_grib2_dict_has(dictionary_01, "wavelength", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "wavelength", &readVal);
    // printf("wavelength: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "123-456") == 0);
    free(readVal);
    readVal = NULL;

    printf(" + iterate through the mars dictionary\n");
    void* iterator = NULL;
    char* key = NULL;
    char* value = NULL;
    ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    for (int i = 0; i < 5; ++i) {
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
    printf("Goodbye from c API (test03) %p, %p\n", iterator, dictionary_01);

    return 0;
}
