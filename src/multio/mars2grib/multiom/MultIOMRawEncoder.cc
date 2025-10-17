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
#include "multio/mars2grib/Mars2GribException.h"

#include "eckit/config/LocalConfiguration.h"

#include "metkit/codes/api/CodesAPI.h"

#include <iostream>

#include <eccodes.h>

namespace std {
template <>
struct default_delete<codes_handle> {
    void operator()(codes_handle* h) { ::codes_handle_delete(h); }
};
}  // namespace std

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


std::unique_ptr<metkit::codes::CodesHandle> MultIOMRawEncoder::allocateAndPreset(
    std::unique_ptr<metkit::codes::CodesHandle> workSample, const MultIOMDict& mars, const MultIOMDict& par,
    const MultIOMDict& geo) {

    std::unique_ptr<codes_handle> h{reinterpret_cast<codes_handle*>(workSample->release())};
    if (multio_grib2_raw_encoder_prepare(encoder_.get(), mars.get(), par.get(), geo.get(), h.get()) != 0) {
        throw Mars2GribException(std::string("Can not prepare grib sample"));
    }

    h = std::unique_ptr<codes_handle>(reinterpret_cast<codes_handle*>(codes_handle_clone(h.get())));
    if (multio_grib2_raw_encoder_allocate(encoder_.get(), mars.get(), par.get(), geo.get(), h.get()) != 0) {
        throw Mars2GribException(std::string("Can not allocate grib sample"));
    }

    h = std::unique_ptr<codes_handle>(reinterpret_cast<codes_handle*>(codes_handle_clone(h.get())));
    if (multio_grib2_raw_encoder_preset(encoder_.get(), mars.get(), par.get(), geo.get(), h.get()) != 0) {
        throw Mars2GribException(std::string("Can not preset grib sample"));
    }

    const void* data;
    size_t size;
    codes_get_message(h.get(), &data, &size);
    return metkit::codes::codesHandleFromMessageCopy(
        metkit::codes::Span<const uint8_t>(reinterpret_cast<const uint8_t*>(data), size));
};

// Applies runtime changes on a prepared samel
// TODO pgeier: Should not take geometry  - will be changed after C++ migration
std::unique_ptr<metkit::codes::CodesHandle> MultIOMRawEncoder::runtime(
    std::unique_ptr<metkit::codes::CodesHandle> workSample, const MultIOMDict& mars, const MultIOMDict& par,
    const MultIOMDict& geo) {
    std::unique_ptr<codes_handle> h{reinterpret_cast<codes_handle*>(workSample->release())};
    if (multio_grib2_raw_encoder_runtime(encoder_.get(), mars.get(), par.get(), geo.get(), h.get()) != 0) {
        throw Mars2GribException(std::string("Can not set runtime data on grib sample"));
    }
    
    const void* data;
    size_t size;
    codes_get_message(h.get(), &data, &size);
    return metkit::codes::codesHandleFromMessageCopy(
        metkit::codes::Span<const uint8_t>(reinterpret_cast<const uint8_t*>(data), size));
}


}  // namespace multio::mars2grib
