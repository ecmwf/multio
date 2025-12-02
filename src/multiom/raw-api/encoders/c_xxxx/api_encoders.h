#pragma once
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif


int multio_grib2_encoder_encode64(void* multio_grib2, void* mars_dict, void* par_dict, const double* data,
                                  size_t data_len, void** out_handle);

int multio_grib2_encoder_encode32(void* multio_grib2, void* mars_dict, void* par_dict, const float* data,
                                  size_t data_len, void** out_handle);


#ifdef __cplusplus
} /* extern "C" */
#endif
