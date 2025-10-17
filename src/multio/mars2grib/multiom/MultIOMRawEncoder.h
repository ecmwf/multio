/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "multio/config/ComponentConfiguration.h"
#include "multio/mars2grib/multiom/MultIOMDict.h"

#include "eckit/config/LocalConfiguration.h"

#include "metkit/codes/api/CodesAPI.h"

#include <memory>

#include "multiom/api/c/api.h"

extern "C" {
int multio_grib2_raw_encoder_open(void* options, void* eckit_conf, void** handle);
int multio_grib2_raw_encoder_close(void** handle);
int multio_grib2_raw_encoder_prepare(void* handle, const void* mars_dict, const void* par_dict, const void* geom_dict,
                                     void* grib_handle);
int multio_grib2_raw_encoder_allocate(void* handle, const void* mars_dict, const void* par_dict, const void* geom_dict,
                                      void* grib_handle);
int multio_grib2_raw_encoder_preset(void* handle, const void* mars_dict, const void* par_dict, const void* geom_dict,
                                    void* grib_handle);
int multio_grib2_raw_encoder_runtime(void* handle, const void* mars_dict, const void* par_dict, const void* geom_dict,
                                     void* grib_handle);
// int multio_grib2_rules_print(void* handle, const char* output_file, int len);
// int multio_grib2_rules_size(void* handle, int64_t* num_rules, int64_t max_linear_size, int64_t* max_levels);
}

namespace multio::mars2grib {
struct ForeignEncoderType;
}

template <>
class std::default_delete<multio::mars2grib::ForeignEncoderType> {
public:
    void operator()(multio::mars2grib::ForeignEncoderType* ptr) const {
        void* p = static_cast<void*>(ptr);
        ASSERT(multio_grib2_raw_encoder_close(&p) == 0);
    }
};


namespace multio::mars2grib {


// New encoder for caching
struct MultIOMRawEncoder {
    MultIOMRawEncoder(const eckit::LocalConfiguration& conf);
    ~MultIOMRawEncoder() = default;

    MultIOMRawEncoder(MultIOMRawEncoder&&) noexcept = default;
    MultIOMRawEncoder& operator=(MultIOMRawEncoder&&) noexcept = default;

    // Calls prepare, allocate and preset on a sample
    // TODO: Should not take parametrization - will be changed after C++ migration
    std::unique_ptr<metkit::codes::CodesHandle> extracted(const void*& data, size_t& size);
    std::unique_ptr<metkit::codes::CodesHandle> allocateAndPreset(std::unique_ptr<metkit::codes::CodesHandle>,
                                                                  const MultIOMDict& mars, const MultIOMDict& par,
                                                                  const MultIOMDict& geo);

    // Applies runtime changes on a prepared samel
    // TODO: Should not take geometry  - will be changed after C++ migration
    std::unique_ptr<metkit::codes::CodesHandle> runtime(std::unique_ptr<metkit::codes::CodesHandle>,
                                                        const MultIOMDict& mars, const MultIOMDict& par,
                                                        const MultIOMDict& geo);

    // std::unique_ptr<codes_handle> encode(MultIOMDict& mars, MultIOMDict& par, const double* data, std::size_t len);
    // std::unique_ptr<codes_handle> encode(MultIOMDict& mars, MultIOMDict& par, const float* data, std::size_t len);

    void* get() const;

    std::unique_ptr<ForeignEncoderType> encoder_;
};


//---------------------------------------------------------------------------------------------------------------------


}  // namespace multio::mars2grib
