#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <functional>


#include "c/api.h"
#include "eccodes.h"

extern "C" {

// prototypes exposed by the fortran layer
int multio_grib2_encoder_encode_f(void* multio_grib2, void* mars_dict, void* par_dict, void** buffer_loc, void* size_loc);


int multio_grib2_encoder_encode64(void* multio_grib2, void* mars_dict, void* par_dict, double* data, size_t data_len,
                                  void** out_handle) {
  void* message = NULL;
  int messageLength = 0;
  int ret = multio_grib2_encoder_encode_f(multio_grib2, mars_dict, par_dict, &message, &messageLength);
  if(ret !=0) return ret;
  if(messageLength ==0) return -1;
  
  std::size_t messageLength2 = messageLength;
  
  *out_handle = codes_handle_new_from_message(NULL, message, messageLength2);
  if (*out_handle == NULL) { return -1; };
  free(message);
  CODES_CHECK(codes_set_double_array((codes_handle*) *out_handle, "values", data, data_len), "Error setting data values on codes handle");
  return ret;
}
                                  
                                  
int multio_grib2_encoder_encode32(void* multio_grib2, void* mars_dict, void* par_dict, float* data, size_t data_len,
                                  void** out_handle) {
    std::vector<double> values{data, data+data_len};
    return multio_grib2_encoder_encode64(multio_grib2, mars_dict, par_dict, values.data(), data_len, out_handle);
}

      
      
}


