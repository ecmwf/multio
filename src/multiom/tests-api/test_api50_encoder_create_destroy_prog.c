#include <stddef.h>
#include <stdio.h>
#include "api.h"

int main() {
    int ret;
    void* optdict = NULL;
    void* multio_grib2_01 = NULL;


    printf("Welcome from c API (test01) %p\n", multio_grib2_01);

    ret = multio_grib2_encoder_open_02(optdict, &multio_grib2_01, "default", 7 );

    printf("Hello from c API (handle 1)  %p\n", multio_grib2_01);

    ret = multio_grib2_encoder_close_02(&multio_grib2_01);

    printf("Goodbye from c API (handle 1)  %p\n", multio_grib2_01);


    return 0;

}