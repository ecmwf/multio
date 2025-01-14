#include <stddef.h>
#include <stdio.h>
#include "api.h"

int main(){

int ret;
void* dictionary_01 = NULL;

printf( "Welcome from c API (test03) %p\n", dictionary_01 );

ret = multio_grib2_dict_create( &dictionary_01, "mars" );

printf( "Set from c API (test01) \n" );
ret = multio_grib2_dict_set( dictionary_01, "stream", "oper" );
ret = multio_grib2_dict_set( dictionary_01, "type", "fc" );
ret = multio_grib2_dict_set( dictionary_01, "class", "rd" );
ret = multio_grib2_dict_set( dictionary_01, "origin", "ecmf" );
ret = multio_grib2_dict_set( dictionary_01, "anoffset", "1234" ); // TODO find a valid offset
ret = multio_grib2_dict_set( dictionary_01, "packing", "ccst" ); 
ret = multio_grib2_dict_set( dictionary_01, "number", "123" ); 
ret = multio_grib2_dict_set( dictionary_01, "ident", "1" ); 
ret = multio_grib2_dict_set( dictionary_01, "instrument", "2" ); 
ret = multio_grib2_dict_set( dictionary_01, "channel", "3" ); 
ret = multio_grib2_dict_set( dictionary_01, "paramType", "optical" ); 
ret = multio_grib2_dict_set( dictionary_01, "chem", "1" );  // TODO find valid chem
ret = multio_grib2_dict_set( dictionary_01, "param", "123" );
ret = multio_grib2_dict_set( dictionary_01, "model", "123" );
ret = multio_grib2_dict_set( dictionary_01, "levtype", "ml" );
ret = multio_grib2_dict_set( dictionary_01, "levelist", "10" );
ret = multio_grib2_dict_set( dictionary_01, "direction", "11" );
ret = multio_grib2_dict_set( dictionary_01, "frequency", "12" );
ret = multio_grib2_dict_set( dictionary_01, "date", "20220812" );
ret = multio_grib2_dict_set( dictionary_01, "time", "800" ); // no starting 0, e.g. 0800 would not be valid
ret = multio_grib2_dict_set( dictionary_01, "step", "8" );
ret = multio_grib2_dict_set( dictionary_01, "repres", "gg" );
ret = multio_grib2_dict_set( dictionary_01, "truncation", "3" ); // TODO find valid truncation
ret = multio_grib2_dict_set( dictionary_01, "timeproc", "3" ); // TODO find valid timeproc
ret = multio_grib2_dict_set( dictionary_01, "expver", "ABC" ); // TODO find valid expver
ret = multio_grib2_dict_set( dictionary_01, "grid", "g" ); // TODO find valid grid

printf( "Destroy from c API (test01) \n" );

ret = multio_grib2_dict_destroy( dictionary_01 );


printf( "Goodbye from c API (test03)\n" );

return 0;

}
