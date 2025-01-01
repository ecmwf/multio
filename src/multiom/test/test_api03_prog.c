#include <stddef.h>
#include <stdio.h>
#include "api.h"

int main(){

int ret;
void* dictionary_01 = NULL;

printf( "Welcome from c API (test03) %p\n", dictionary_01 );

ret = multio_grib2_dict_create( &dictionary_01, "mars" );

printf( "Set from c API (test01) \n" );
ret = multio_grib2_dict_set( dictionary_01, "mars", "asfsdfghjfjlghldkjsghls" );

printf( "Destroy from c API (test01) \n" );

ret = multio_grib2_dict_destroy( dictionary_01 );


printf( "Goodbye from c API (test03)\n" );

return 0;

}