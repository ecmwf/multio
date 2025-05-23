#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <functional>
#include <iomanip>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>


#include "c/api.h"
#include "eccodes.h"

extern "C" {

// prototypes exposed by the fortran layer
int multio_grib2_encoder_encode_f(void* multio_grib2, void* mars_dict, void* par_dict, void** buffer_loc,
                                  void* size_loc);


int multio_grib2_encoder_encode64(void* multio_grib2, void* mars_dict, void* par_dict, const double* data,
                                  size_t data_len, void** out_handle) {
    void* message = NULL;
    int messageLength = 0;
    int ret = multio_grib2_encoder_encode_f(multio_grib2, mars_dict, par_dict, &message, &messageLength);
    if (ret != 0) {
        return ret;
    }
    if (messageLength == 0) {
        return -1;
    }

    std::size_t messageLength2 = messageLength;

    *out_handle = codes_handle_new_from_message_copy(NULL, message, messageLength2);
    if (*out_handle == NULL) {
        return -1;
    };
    free(message);

    char* scaleFactorCStr = NULL;
    std::optional<double> scaleFactor;
    ret = multio_grib2_dict_get(par_dict, "values-scale-factor", &scaleFactorCStr);
    if (ret != 0) {
        return ret;
    }
    if (scaleFactorCStr != NULL) {
        scaleFactor = std::stod(scaleFactorCStr);
        free(scaleFactorCStr);
    }

    char* missingValuesCount = "0";
    ret = multio_grib2_dict_get(par_dict, "number-of-missing-values", &missingValuesCount);
    long bitmapPresent = static_cast<long>(std::stol(missingValuesCount) > 0);
    CODES_CHECK(codes_set_long((codes_handle*)*out_handle, "bitmapPresent", bitmapPresent),
                "Error setting bitmapPresent");

    auto missingValue = static_cast<double>(std::numeric_limits<float>::max());
    if (bitmapPresent) {
        char* strMissingValue;
        ret = multio_grib2_dict_get(par_dict, "value-of-missing-values", &strMissingValue);
        missingValue = std::stod(strMissingValue);
        CODES_CHECK(codes_set_double((codes_handle*)*out_handle, "missingValue", missingValue),
                    "Error setting missingValue");
    }

    std::vector<double> scaledValues;
    if (scaleFactor && (*scaleFactor != 1.0)) {
        scaledValues.reserve(data_len);
        std::transform(data, data + data_len, std::back_inserter(scaledValues),
                       [&scaleFactor, missingValue](const double& v) {
                           return (v == missingValue) ? missingValue : (v * (*scaleFactor));
                       });
        data = scaledValues.data();
    }

    CODES_CHECK(codes_set_double_array((codes_handle*)*out_handle, "values", data, data_len),
                "Error setting data values on codes handle");
    return ret;
}


int multio_grib2_encoder_encode32(void* multio_grib2, void* mars_dict, void* par_dict, const float* data,
                                  size_t data_len, void** out_handle) {
    std::vector<double> values{data, data + data_len};
    return multio_grib2_encoder_encode64(multio_grib2, mars_dict, par_dict, values.data(), data_len, out_handle);
}
}
