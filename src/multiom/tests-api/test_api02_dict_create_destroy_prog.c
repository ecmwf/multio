#include <stddef.h>
#include <stdio.h>
#include "api.h"

int main(){
int ret;
void* dictionary_01 = NULL;
void* dictionary_02 = NULL;
void* dictionary_03 = NULL;

printf( "Welcome from c API (test02) %p\n", dictionary_01 );

ret = multio_grib2_dict_create( &dictionary_01, "mars" );
ret = multio_grib2_dict_create( &dictionary_02, "parametrization" );
// ret = multio_grib2_dict_create( &dictionary_03, "options" );


printf( "Hello from c API (test01) \n" );

ret = multio_grib2_dict_destroy( &dictionary_02 );
ret = multio_grib2_dict_destroy( &dictionary_01 );
// ret = multio_grib2_dict_create( &dictionary_03, "options" );

printf( "Goodbye from c API (test02)\n" );

return 0;

}