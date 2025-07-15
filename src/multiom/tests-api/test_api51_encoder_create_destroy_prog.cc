#include <stddef.h>
#include <stdio.h>
#include "api.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"


extern "C" {
    int multio_grib2_raw_encoder_open( void* opt_dict, void* YAML, void** multio_grib2 );
int multio_grib2_raw_encoder_prepare( void* handle, const void* mars_dict, const void* par_dict, const void* geom_dict,
                                     void* grib_handle);
int multio_grib2_raw_encoder_allocate( void* handle, const void* mars_dict, const void* par_dict, const void* geom_dict,
                                      void* grib_handle);
int multio_grib2_raw_encoder_preset( void* handle, const void* mars_dict, const void* par_dict, const void* geom_dict,
                                    void* grib_handle);
int multio_grib2_raw_encoder_runtime( void* handle, const void* mars_dict, const void* par_dict, const void* geom_dict,
                                     void* grib_handle);
    int multio_grib2_raw_encoder_close( void** multio_grib2 );
}


int main() {
    int ret;
    void* optdict;
    void* multio_grib2 = NULL;
    eckit::LocalConfiguration recipe;
    eckit::LocalConfiguration rule = eckit::LocalConfiguration{eckit::YAMLConfiguration{eckit::PathName{"./rule.yaml"}}};
    if ( rule.has("encoder") ) {
      recipe = rule.getSubConfiguration("encoder");
      std::cout << recipe << std::endl;
    }
    else {
      printf("No rule found in rule.yaml\n");
      return 1;
    }

    printf("Welcome from c API (test01) %p\n", multio_grib2);

    optdict = NULL;
    ret = multio_grib2_raw_encoder_open( optdict, static_cast<void*>(&recipe), &multio_grib2 );

    printf("Hello from c API (handle 1)  %p\n", multio_grib2);

    ret = multio_grib2_raw_encoder_close(&multio_grib2);

    printf("Goodbye from c API (handle 1)  %p\n", multio_grib2);

    return 0;
}