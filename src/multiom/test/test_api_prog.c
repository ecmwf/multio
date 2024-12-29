#include <stddef.h>
#include <stdio.h>
#include "api.h"

int main(){

void* optdict;
void* multio_grib2;

optdict = NULL;
multio_grib2 = NULL;
int ret = multio_grib2_encoder_open( optdict, &multio_grib2 );

printf( "Hello world from c API\n" );

return 0;

}