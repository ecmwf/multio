#include <stddef.h>
#include <stdio.h>
#include "api.h"

int main() {
    int ret;
    void* optdict = NULL;
    void* multio_grib2_01 = NULL;
    void* multio_grib2_02 = NULL;
    void* multio_grib2_03 = NULL;
    void* multio_grib2_04 = NULL;
    void* multio_grib2_05 = NULL;
    void* multio_grib2_06 = NULL;
    void* multio_grib2_07 = NULL;
    void* multio_grib2_08 = NULL;
    void* multio_grib2_09 = NULL;
    void* multio_grib2_10 = NULL;


    printf("Welcome from c API (test01) %p\n", multio_grib2_01);

    ret = multio_grib2_encoder_open_02(optdict, &multio_grib2_01, "default", 7 );

    printf("Hello from c API (handle 1)  %p\n", multio_grib2_01);

    ret = multio_grib2_encoder_close_02(&multio_grib2_10);

    printf("Goodbye from c API (handle 1)  %p\n", multio_grib2_01);


    return 0;
}