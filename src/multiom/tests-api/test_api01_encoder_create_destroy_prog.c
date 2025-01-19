#include <stddef.h>
#include <stdio.h>
#include "api.h"

int main(){
int ret;
void* optdict;
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


printf( "Welcome from c API (test01) %p\n", multio_grib2_01 );

optdict = NULL;
ret = multio_grib2_encoder_open( optdict, &multio_grib2_01 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_02 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_03 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_04 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_05 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_06 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_07 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_08 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_09 );
ret = multio_grib2_encoder_open( optdict, &multio_grib2_10 );

printf( "Hello from c API (test01) %p\n", multio_grib2_01 );

ret = multio_grib2_encoder_close( &multio_grib2_10 );
ret = multio_grib2_encoder_close( &multio_grib2_09 );
ret = multio_grib2_encoder_close( &multio_grib2_08 );
ret = multio_grib2_encoder_close( &multio_grib2_07 );
ret = multio_grib2_encoder_close( &multio_grib2_06 );
ret = multio_grib2_encoder_close( &multio_grib2_05 );
ret = multio_grib2_encoder_close( &multio_grib2_04 );
ret = multio_grib2_encoder_close( &multio_grib2_03 );
ret = multio_grib2_encoder_close( &multio_grib2_02 );
ret = multio_grib2_encoder_close( &multio_grib2_01 );

printf( "Goodbye from c API (test01) \n" );


return 0;

}