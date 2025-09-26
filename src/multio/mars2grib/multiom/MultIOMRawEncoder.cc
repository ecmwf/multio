/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/mars2grib/multiom/MultIOMRawEncoder.h"
#include "eckit/config/LocalConfiguration.h"
#include "multio/mars2grib/Mars2GribException.h"

#include <iostream>

#include "multio/util/MioGribHandle.h"

namespace multio::mars2grib {


MultIOMRawEncoder::MultIOMRawEncoder(const eckit::LocalConfiguration& conf) {
    void* handle = NULL;
    if (multio_grib2_raw_encoder_open(
            nullptr,
            static_cast<void*>(const_cast<eckit::Configuration*>(static_cast<const eckit::Configuration*>(&conf))),
            &handle)
        != 0) {
        std::ostringstream oss;
        oss << "Can not create encoder from conf: " << conf;
        throw Mars2GribException(oss.str(), Here());
    }

    encoder_.reset(static_cast<ForeignEncoderType*>(handle));
}

void* MultIOMRawEncoder::get() const {
    return static_cast<void*>(encoder_.get());
}


std::unique_ptr<util::MioGribHandle> MultIOMRawEncoder::allocateAndPreset(std::unique_ptr<util::MioGribHandle> workSample,
                                                                const MultIOMDict& mars, const MultIOMDict& par,
                                                                const MultIOMDict& geo) {
    if (multio_grib2_raw_encoder_prepare(encoder_.get(), mars.get(), par.get(), geo.get(), workSample->raw()) != 0) {
        throw Mars2GribException(std::string("Can not prepare grib sample"));
    }

    workSample = workSample->duplicate();  // Safe reload to avoid eccodes setting artifacts....
    if (multio_grib2_raw_encoder_allocate(encoder_.get(), mars.get(), par.get(), geo.get(), workSample->raw()) != 0) {
        throw Mars2GribException(std::string("Can not allocate grib sample"));
    }

    workSample = workSample->duplicate();  // Safe reload to avoid eccodes setting artifacts....
    if (multio_grib2_raw_encoder_preset(encoder_.get(), mars.get(), par.get(), geo.get(), workSample->raw()) != 0) {
        throw Mars2GribException(std::string("Can not preset grib sample"));
    }

    workSample = workSample->duplicate();  // Safe reload to force eccodes freeing memory
    return workSample;
};

// Applies runtime changes on a prepared samel
// TODO pgeier: Should not take geometry  - will be changed after C++ migration
std::unique_ptr<util::MioGribHandle> MultIOMRawEncoder::runtime(std::unique_ptr<util::MioGribHandle> workSample,
                                                                const MultIOMDict& mars, const MultIOMDict& par,
                                                                const MultIOMDict& geo) {
    if (multio_grib2_raw_encoder_runtime(encoder_.get(), mars.get(), par.get(), geo.get(), workSample->raw()) != 0) {
        throw Mars2GribException(std::string("Can not set runtime data on grib sample"));
    }
    return workSample;
}


}  // namespace multio::mars2grib
