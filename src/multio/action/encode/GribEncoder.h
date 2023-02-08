/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany

/// @date Aug 2020

#pragma once

#include "eccodes.h"

#include "metkit/codes/GribHandle.h"

#include "multio/message/Message.h"

// #include "multio/ifsio/EncodeBitsPerValue.h"

namespace multio {
namespace action {

class GribEncoder : public metkit::grib::GribHandle {
public:
    GribEncoder(codes_handle* handle, const eckit::LocalConfiguration& config);

    bool gridInfoReady(const std::string& subtype) const;
    bool setGridInfo(message::Message msg);

    void setValue(const std::string& key, long value);
    void setValue(const std::string& key, double value);
    void setValue(const std::string& key, const std::string& value);
    void setValue(const std::string& key, const unsigned char* value);


    // Convert bool to long (0/1)
    void setValue(const std::string& key, bool value);

    template <typename T>
    void setValue(const std::string& key, eckit::Optional<T> v) {
        if (v) {
            setValue(key, *v);
        }
    }

    message::Message encodeOceanLatitudes(const std::string& subtype);
    message::Message encodeOceanLongitudes(const std::string& subtype);

    message::Message encodeField(const message::Message& msg);
    message::Message encodeField(const message::Message& msg, const double* data, size_t sz);
    message::Message encodeField(const message::Message& msg, const float* data, size_t sz);

    // TODO May be refactored
    // int getBitsPerValue(int paramid, const std::string& levtype, double min, double max);

    void print(std::ostream& os) const;

private:
    void setFieldMetadata(const message::Message& msg);
    void setOceanMetadata(const message::Message& msg);

    void setOceanCoordMetadata(const message::Metadata& metadata);
    void setOceanCoordMetadata(const message::Metadata& metadata, const eckit::Configuration& runConfig);

    // TODO: these functions have to be removed when the
    // single pricision support will be added to eccodes
    using metkit::grib::GribHandle::setDataValues;
    void setDataValues(const float*, size_t);

    template <typename T>
    message::Message setFieldValues(const message::Message& msg);



    message::Message setFieldValues(const double* values, size_t count);
    message::Message setFieldValues(const float* values, size_t count);
    
    const eckit::LocalConfiguration config_;

    const std::set<std::string> coordSet_{"lat_T", "lon_T", "lat_U", "lon_U", "lat_V",
                                          "lon_V", "lat_W", "lon_W", "lat_F", "lon_F"};

    // TODO: This is just included from old interface now and may require refactoring in terms of configuration and its
    // action EncodeBitsPerValue encodeBitsPerValue_;
};

inline bool isOcean(const message::Metadata& metadata) {
    // Check if metadata has a key "nemoParam" or a category starting with "ocean"
    return metadata.has("nemoParam")
        || (metadata.has("category") && (metadata.getString("category").rfind("ocean") == 0));
};


}  // namespace action
}  // namespace multio
