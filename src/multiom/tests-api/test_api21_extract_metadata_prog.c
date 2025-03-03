#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "api.h"
#include "eccodes.h"


//  ./bin/multiom_test_api21_extract_metadata.x /perm/maro/reference_grib_files/151_od_enfh_pf_sfc.grib1


int main(int argc, char* argv[]) {

    int ret;
    codes_handle* h = NULL;

    FILE* fp;

    void* mars_dict = NULL;
    void* par_dict = NULL;

    char* readVal = NULL;
    void* iterator = NULL;
    char* key = NULL;
    char* value = NULL;

    if (argc != 2) {
        perror("Expecting a filepath as commandline argument");
        return 2;
    }

    if ((fp = fopen(argv[1], "rb")) == NULL) {
        perror("fopen()");
        return 3;
    }

    h = codes_grib_handle_new_from_file(NULL, fp, &ret);

    if (ret != 0) {
        perror("Can not create grib message from file");
        return 4;
    }

    // printf("mars dict before: %p\n", mars_dict);
    ret = multio_grib2_encoder_extract_metadata(NULL, h, &mars_dict, &par_dict);
    if (ret != 0) {
        perror("Can not extract metadata");
        return 5;
    }

    // printf("mars dict after: %p\n", mars_dict);
    ret = multio_grib2_dict_to_yaml(mars_dict, "stdout");
    if (ret != 0) {
        perror("Can not print mars dict");
        return 6;
    }

    ret = multio_grib2_dict_to_yaml(par_dict, "stdout");
    if (ret != 0) {
        perror("Can not print parametrization dict");
        return 7;
    }

    return 0;
}
