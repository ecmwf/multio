#include <stddef.h>
#include <stdio.h>
#include "api.h"

int main() {
    int ret;
    void* dictionary_01 = NULL;
    void* dictionary_02 = NULL;
    void* dictionary_03 = NULL;
    void* dictionary_04 = NULL;

    printf("Welcome from c API (%s)\n", __FILE__);

    ret = multio_grib2_dict_create(&dictionary_01, "mars");
    printf("mars dictionary created: %d\n", ret);
    ret = multio_grib2_dict_create(&dictionary_02, "parametrization");
    printf("parametrization dictionary created: %d\n", ret);
    ret = multio_grib2_dict_create(&dictionary_03, "geometry-reduced-gg");
    printf("reduced-gg dictionary created: %d\n", ret);
    ret = multio_grib2_dict_create(&dictionary_04, "geometry-sh");
    printf("sh dictionary created: %d\n", ret);


    printf("Hello from c API (test01) \n");

    ret = multio_grib2_dict_destroy(&dictionary_04);
    printf("sh dictionary destroied: %d, %p\n", ret, dictionary_04);
    ret = multio_grib2_dict_destroy(&dictionary_03);
    printf("reduced-gg dictionary destroied: %d, %p\n", ret, dictionary_03);
    ret = multio_grib2_dict_destroy(&dictionary_02);
    printf("parametrization dictionary destroied: %d, %p\n", ret, dictionary_02);
    ret = multio_grib2_dict_destroy(&dictionary_01);
    printf("mars dictionary destroied: %d, %p\n", ret, dictionary_01);


    printf("Goodbye from c API (test02)\n");

    return 0;
}