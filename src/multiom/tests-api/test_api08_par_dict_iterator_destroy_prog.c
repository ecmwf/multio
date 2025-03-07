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

    ret = multio_grib2_dict_create(&dictionary_01, "parametrization");

    printf("Set from c API (test03 PARAMETRIZATION keys) \n");

    ret = multio_grib2_dict_has(dictionary_01, "tablesversion", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "tablesversion", "42");
    ret = multio_grib2_dict_has(dictionary_01, "tablesversion", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "tablesversion", &readVal);
    // printf("tablesversion: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "42") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "generatingprocessidentifier", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "generatingprocessidentifier", "52");
    ret = multio_grib2_dict_has(dictionary_01, "generatingprocessidentifier", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "generatingprocessidentifier", &readVal);
    // printf("generatingprocessidentifier: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "52") == 0);
    free(readVal);
    readVal = NULL;
    
    ret = multio_grib2_dict_has(dictionary_01, "typeofprocesseddata", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "typeofprocesseddata", "10");
    ret = multio_grib2_dict_has(dictionary_01, "typeofprocesseddata", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "typeofprocesseddata", &readVal);
    // printf("typeofprocesseddata: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "10") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "initialstep", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "initialstep", "2");
    ret = multio_grib2_dict_has(dictionary_01, "initialstep", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "initialstep", &readVal);
    // printf("initialstep: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "2") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "lengthoftimestepinseconds", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "lengthoftimestepinseconds", "60");
    ret = multio_grib2_dict_has(dictionary_01, "lengthoftimestepinseconds", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "lengthoftimestepinseconds", &readVal);
    // printf("lengthoftimestepinseconds: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "60") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "lengthoftimerangeinseconds", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "lengthoftimerangeinseconds", "3600");
    ret = multio_grib2_dict_has(dictionary_01, "lengthoftimerangeinseconds", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "lengthoftimerangeinseconds", &readVal);
    // printf("lengthoftimerangeinseconds: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "3600") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "valuesscalefactor", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "valuesscalefactor", "1000.1");
    ret = multio_grib2_dict_has(dictionary_01, "valuesscalefactor", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "valuesscalefactor", &readVal);
    // printf("valuesscalefactor: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "1.000100e+03") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "numberofmissingvalues", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "numberofmissingvalues", "1002");
    ret = multio_grib2_dict_has(dictionary_01, "numberofmissingvalues", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "numberofmissingvalues", &readVal);
    // printf("numberofmissingvalues: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "1002") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "valueofmissingvalues", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "valueofmissingvalues", "99.99");
    ret = multio_grib2_dict_has(dictionary_01, "valueofmissingvalues", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "valueofmissingvalues", &readVal);
    // printf("valueofmissingvalues: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "9.999000e+01") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "systemnumber", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "systemnumber", "99");
    ret = multio_grib2_dict_has(dictionary_01, "systemnumber", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "systemnumber", &readVal);
    // printf("systemnumber: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "99") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "methodnumber", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "methodnumber", "98");
    ret = multio_grib2_dict_has(dictionary_01, "methodnumber", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "methodnumber", &readVal);
    // printf("methodnumber: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "98") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "typeofensembleforecast", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "typeofensembleforecast", "3");
    ret = multio_grib2_dict_has(dictionary_01, "typeofensembleforecast", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "typeofensembleforecast", &readVal);
    // printf("typeofensembleforecast: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "3") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "lengthoftimewindow", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "lengthoftimewindow", "5");
    ret = multio_grib2_dict_has(dictionary_01, "lengthoftimewindow", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "lengthoftimewindow", &readVal);
    // printf("lengthoftimewindow: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "5") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "bitspervalue", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "bitspervalue", "16");
    ret = multio_grib2_dict_has(dictionary_01, "bitspervalue", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "bitspervalue", &readVal);
    // printf("bitspervalue: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "16") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "pv", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "pv", "[1.2, 2.3, 4.5]");
    ret = multio_grib2_dict_has(dictionary_01, "pv", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "pv", &readVal);
    // printf("pv: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "[ 1.200000e+00, 2.300000e+00, 4.500000e+00 ]") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "wave-frequencies", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "wave-frequencies", "[1.2, 2.3, 4.5]");
    ret = multio_grib2_dict_has(dictionary_01, "wave-frequencies", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "wave-frequencies", &readVal);
    // printf("wave-frequencies: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "[ 1.200000e+00, 2.300000e+00, 4.500000e+00 ]") == 0);
    free(readVal);
    readVal = NULL;

    ret = multio_grib2_dict_has(dictionary_01, "wave-directions", &has1);
    ret = multio_grib2_dict_set(dictionary_01, "wave-directions", "[1.2, 2.3, 4.5, 5.6 ]");
    ret = multio_grib2_dict_has(dictionary_01, "wave-directions", &has2);
    ret = multio_grib2_dict_get(dictionary_01, "wave-directions", &readVal);
    // printf("wave-directions: %s, %d, %d, %d\n", readVal, has1, has2, ret );
    assert(strcmp(readVal, "[ 1.200000e+00, 2.300000e+00, 4.500000e+00, 5.600000e+00 ]") == 0);
    free(readVal);
    readVal = NULL;


    printf(" + iterate through the mars dictionary\n");
    void* iterator = NULL;
    char* key = NULL;
    char* value = NULL;
    ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    for (int i = 0; i < 6; ++i) {
        printf("Iterate: \"%s\": \"%s\"\n", key, value);
        free(key);
        free(value);
        ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    };

    printf("Destroy iterator from c API (test01) %p, %p \n", iterator, dictionary_01);
    ret = multio_grib2_dict_destroy_iterator(dictionary_01, &iterator);

    printf("\n\n\n\n\n\n");
    printf(" + iterate through the mars dictionary\n");
    ret = multio_grib2_dict_iterate(dictionary_01, &iterator, &key, &value);
    for (int i = 0; i < 6; ++i) {
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
