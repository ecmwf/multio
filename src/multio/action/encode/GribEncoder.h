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

#include <memory>

#include "eckit/config/LocalConfiguration.h"

#include "eccodes.h"
#include "metkit/codes/GribHandle.h"

#include "multio/message/Glossary.h"
#include "multio/message/Message.h"
#include "multio/util/MioGribHandle.h"

#include "multio/util/MioGribHandle.h"


namespace multio::action {

using multio::util::MioGribHandle;

class GribEncoder {
public:
    GribEncoder(codes_handle* handle, const eckit::LocalConfiguration& config);

    template <typename T>
    void setValue(const std::string& key, std::optional<T> v) {
        if (v) {
            encoder_->setValue(key, *v);
        }
    };

    template <typename T>
    void setValue(const std::string& key, T v) {
        if constexpr (std::is_signed_v<T> && std::is_integral_v<T>) {
            encoder_->setValue(key, static_cast<std::int64_t>(v));
        }
        else {
            encoder_->setValue(key, v);
        }
    };

    void setMissing(const std::string& key);

    template <typename T>
    void setValue(const std::string& key, const std::vector<T>& v) {
        encoder_->setValue(key, v);
    };
    template <typename T>
    void setDataValues(const T* data, size_t count) {
        encoder_->setDataValues(data, count);
    };
    bool hasKey(const char* key);

    message::Message encodeOceanCoordinates(message::Message&& msg);

    message::Message encodeField(const message::Message& msg);
    message::Message encodeField(const message::Message& msg, const double* data, size_t sz);
    message::Message encodeField(const message::Message& msg, const float* data, size_t sz);

    // TODO May be refactored
    // int getBitsPerValue(int paramid, const std::string& levtype, double min, double max);

    void print(std::ostream& os) const;

private:
    // Encoder is now a member of the action
    const MioGribHandle template_;
    std::unique_ptr<MioGribHandle> encoder_;

    void initEncoder();

    void setFieldMetadata(const message::Message& msg);
    void setOceanMetadata(const message::Message& msg);

    void setOceanCoordMetadata(const message::Metadata& metadata);
    void setOceanCoordMetadata(const message::Metadata& metadata, const eckit::Configuration& runConfig);

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
    using message::glossary;

    // Check if metadata has a key "nemoParam" or a category starting with "ocean"
    std::optional<std::string> category;
    return (metadata.find(glossary().nemoParam) != metadata.end())
        || ((category = metadata.getOpt<std::string>(glossary().category)) && (category->rfind("ocean") == 0));
};


}  // namespace multio::action
