#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "api.h"

int main(){

int ret;
void* dictionary_01 = NULL;
void* dictionary_02 = NULL;
char* readVal = NULL;

printf( "Welcome from c API (test03) %p\n", dictionary_01 );

ret = multio_grib2_dict_create( &dictionary_01, "mars" );

printf( "Set from c API (test03 MARS keys) \n" );

ret = multio_grib2_dict_set( dictionary_01, "stream", "oper" );
ret = multio_grib2_dict_get( dictionary_01, "stream", &readVal );
printf("stream: %s\n", readVal);
assert(strcmp(readVal, "oper") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "type", "fc" );
ret = multio_grib2_dict_get( dictionary_01, "type", &readVal );
printf("type: %s\n", readVal);
assert(strcmp(readVal, "fc") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "class", "rd" );
ret = multio_grib2_dict_get( dictionary_01, "class", &readVal );
printf("class: %s\n", readVal);
assert(strcmp(readVal, "rd") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "origin", "ecmf" );
ret = multio_grib2_dict_get( dictionary_01, "origin", &readVal );
printf("origin: %s\n", readVal);
assert(strcmp(readVal, "ecmf") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "anoffset", "1234" ); // TODO find a valid offset
ret = multio_grib2_dict_get( dictionary_01, "anoffset", &readVal ); // TODO find a valid offset
printf("anoffset: %s\n", readVal);
assert(strcmp(readVal, "1234") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "packing", "ccsds" ); 
ret = multio_grib2_dict_get( dictionary_01, "packing", &readVal ); 
printf("packing: %s\n", readVal);
assert(strcmp(readVal, "ccsds") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "number", "123" ); 
ret = multio_grib2_dict_get( dictionary_01, "number", &readVal ); 
printf("number: %s\n", readVal);
assert(strcmp(readVal, "123") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "ident", "1" ); 
ret = multio_grib2_dict_get( dictionary_01, "ident", &readVal ); 
printf("ident: %s\n", readVal);

ret = multio_grib2_dict_set( dictionary_01, "instrument", "2" ); 
ret = multio_grib2_dict_get( dictionary_01, "instrument", &readVal ); 
printf("instrument: %s\n", readVal);
assert(strcmp(readVal, "2") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "channel", "3" ); 
ret = multio_grib2_dict_get( dictionary_01, "channel", &readVal ); 
printf("channel: %s\n", readVal);
assert(strcmp(readVal, "3") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "paramType", "optical" ); 
ret = multio_grib2_dict_get( dictionary_01, "paramType", &readVal ); 
printf("paramType: %s\n", readVal);
assert(strcmp(readVal, "optical") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "chem", "1" );  // TODO find valid chem
ret = multio_grib2_dict_get( dictionary_01, "chem", &readVal );  // TODO find valid chem
printf("chem: %s\n", readVal);
assert(strcmp(readVal, "1") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "param", "123" );
ret = multio_grib2_dict_get( dictionary_01, "param", &readVal );
printf("param: %s\n", readVal);
assert(strcmp(readVal, "123") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "model", "atmosphere" );
ret = multio_grib2_dict_get( dictionary_01, "model", &readVal );
printf("model: %s\n", readVal);
assert(strcmp(readVal, "atmosphere") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "levtype", "ml" );
ret = multio_grib2_dict_get( dictionary_01, "levtype", &readVal );
printf("levtype: %s\n", readVal);
assert(strcmp(readVal, "ml") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "levelist", "10" );
ret = multio_grib2_dict_get( dictionary_01, "levelist", &readVal );
printf("levelist: %s\n", readVal);
assert(strcmp(readVal, "10") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "direction", "11" );
ret = multio_grib2_dict_get( dictionary_01, "direction", &readVal );
printf("direction: %s\n", readVal);
assert(strcmp(readVal, "11") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "frequency", "12" );
ret = multio_grib2_dict_get( dictionary_01, "frequency", &readVal );
printf("frequency: %s\n", readVal);
assert(strcmp(readVal, "12") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "date", "20220812" );
ret = multio_grib2_dict_get( dictionary_01, "date", &readVal );
printf("date: %s\n", readVal);
assert(strcmp(readVal, "20220812") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "time", "800" ); // no starting 0, e.g. 0800 would not be valid
ret = multio_grib2_dict_get( dictionary_01, "time", &readVal ); // no starting 0, e.g. 0800 would not be valid
printf("time: %s\n", readVal);
assert(strcmp(readVal, "800") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "step", "8" );
ret = multio_grib2_dict_get( dictionary_01, "step", &readVal );
printf("step: %s\n", readVal);
assert(strcmp(readVal, "8") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "repres", "gg" );
ret = multio_grib2_dict_get( dictionary_01, "repres", &readVal );
printf("repres: %s\n", readVal);
assert(strcmp(readVal, "gg") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "truncation", "3" ); // TODO find valid truncation
ret = multio_grib2_dict_get( dictionary_01, "truncation", &readVal ); // TODO find valid truncation
printf("truncation: %s\n", readVal);
assert(strcmp(readVal, "3") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "timeproc", "3" ); // TODO find valid timeproc
ret = multio_grib2_dict_get( dictionary_01, "timeproc", &readVal ); // TODO find valid timeproc
printf("timeproc: %s\n", readVal);
assert(strcmp(readVal, "3") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "expver", "ABC" ); // TODO find valid expver
ret = multio_grib2_dict_get( dictionary_01, "expver", &readVal ); // TODO find valid expver
printf("expver: %s\n", readVal);
assert(strcmp(readVal, "ABC") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_01, "grid", "g" ); // TODO find valid grid
ret = multio_grib2_dict_get( dictionary_01, "grid", &readVal ); // TODO find valid grid
printf("grid: %s\n", readVal);
assert(strcmp(readVal, "g") == 0);
free(readVal);

printf( "Destroy from c API (test01) \n" );

ret = multio_grib2_dict_destroy( dictionary_01 );

dictionary_01 = NULL;


ret = multio_grib2_dict_create( &dictionary_02, "parametrization" );

printf( "Set from c API (test03 PARAMETRIZATION keys) \n" );

ret = multio_grib2_dict_set( dictionary_02, "tablesversion", "42" );
ret = multio_grib2_dict_get( dictionary_02, "tablesversion", &readVal );
printf("tablesversion: %s\n", readVal);
assert(strcmp(readVal, "42") == 0);
free(readVal);
 
ret = multio_grib2_dict_set( dictionary_02, "generatingprocessidentifier", "52" );
ret = multio_grib2_dict_get( dictionary_02, "generatingprocessidentifier", &readVal );
printf("generatingprocessidentifier: %s\n", readVal);
assert(strcmp(readVal, "52") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_02, "initialstep", "2" );
ret = multio_grib2_dict_get( dictionary_02, "initialstep", &readVal );
printf("initialstep: %s\n", readVal);
assert(strcmp(readVal, "2") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_02, "lengthoftimestepinseconds", "60" );
ret = multio_grib2_dict_get( dictionary_02, "lengthoftimestepinseconds", &readVal );
printf("lengthoftimestepinseconds: %s\n", readVal);
assert(strcmp(readVal, "60") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_02, "lengthoftimerangeinseconds", "3600" );
ret = multio_grib2_dict_get( dictionary_02, "lengthoftimerangeinseconds", &readVal );
printf("lengthoftimerangeinseconds: %s\n", readVal);
assert(strcmp(readVal, "3600") == 0);
free(readVal);

ret = multio_grib2_dict_set( dictionary_02, "valuesscalefactor", "1000.1" );
ret = multio_grib2_dict_get( dictionary_02, "valuesscalefactor", &readVal );
printf("valuesscalefactor: %s\n", readVal);
assert(strcmp(readVal, "1000.1") == 0);
free(readVal);


ret = multio_grib2_dict_set( dictionary_02, "numberofmissingvalues", "1002" );
ret = multio_grib2_dict_get( dictionary_02, "numberofmissingvalues", &readVal );
printf("numberofmissingvalues: %s\n", readVal);
assert(strcmp(readVal, "1002") == 0);
free(readVal);


ret = multio_grib2_dict_set( dictionary_02, "valueofmissingvalues", "99.99" );
ret = multio_grib2_dict_get( dictionary_02, "valueofmissingvalues", &readVal );
printf("valueofmissingvalues: %s\n", readVal);
assert(strcmp(readVal, "99.99") == 0);
free(readVal);


ret = multio_grib2_dict_set( dictionary_02, "systemnumber", "99" );
ret = multio_grib2_dict_get( dictionary_02, "systemnumber", &readVal );
printf("systemnumber: %s\n", readVal);
assert(strcmp(readVal, "99") == 0);
free(readVal);


ret = multio_grib2_dict_set( dictionary_02, "methodnumber", "98" );
ret = multio_grib2_dict_get( dictionary_02, "methodnumber", &readVal );
printf("methodnumber: %s\n", readVal);
assert(strcmp(readVal, "98") == 0);
free(readVal);


ret = multio_grib2_dict_set( dictionary_02, "typeofensembleforecast", "3" );
ret = multio_grib2_dict_get( dictionary_02, "typeofensembleforecast", &readVal );
printf("typeofensembleforecast: %s\n", readVal);
assert(strcmp(readVal, "3") == 0);
free(readVal);


ret = multio_grib2_dict_set( dictionary_02, "lengthoftimewindow", "5" );
ret = multio_grib2_dict_get( dictionary_02, "lengthoftimewindow", &readVal );
printf("lengthoftimewindow: %s\n", readVal);
assert(strcmp(readVal, "5") == 0);
free(readVal);


ret = multio_grib2_dict_set( dictionary_02, "bitspervalue", "16" );
ret = multio_grib2_dict_get( dictionary_02, "bitspervalue", &readVal );
printf("bitspervalue: %s\n", readVal);
assert(strcmp(readVal, "16") == 0);
free(readVal);


ret = multio_grib2_dict_set( dictionary_02, "levels", "[1.2, 2.3, 4.5]" );
ret = multio_grib2_dict_get( dictionary_02, "levels", &readVal );
printf("levels: %s\n", readVal);
assert(strcmp(readVal, "[1.2, 2.3, 4.5]") == 0);
free(readVal);

printf( "Destroy from c API (test02) \n" );

ret = multio_grib2_dict_destroy( dictionary_02 );


printf( "Goodbye from c API (test03)\n" );

return 0;

}
