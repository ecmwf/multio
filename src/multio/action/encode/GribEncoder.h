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

#include "MioGribHandle.h"
#include "eccodes.h"
#include "metkit/codes/GribHandle.h"

#include "multio/message/Message.h"

#include <variant>


namespace multio::action {

using CodesScalarValue = std::variant<std::int64_t, double, std::string>;
using CodesOverwrites = std::vector<std::pair<std::string, CodesScalarValue>>;

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
            encoder_->setValue(key, static_cast<long>(v));
        }
        else {
            encoder_->setValue(key, v);
        }
    };

    void setMissing(const std::string& key);

    template <typename T>
    void setDataValues(const T* data, size_t count) {
        encoder_->setDataValues(data, count);
    };
    bool hasKey(const char* key);

    message::Message encodeOceanCoordinates(message::Message&& msg,
                                            const eckit::LocalConfiguration& additionalMetadata);

    message::Message encodeField(const message::Message& msg, const CodesOverwrites& overwrites,
                                 const eckit::LocalConfiguration& additionalMetadata);

    // TODO May be refactored
    // int getBitsPerValue(int paramid, const std::string& levtype, double min, double max);

    void print(std::ostream& os) const;

private:
    // Encoder is now a member of the action
    const MioGribHandle template_;
    std::unique_ptr<MioGribHandle> encoder_;

    void initEncoder();

    void setFieldMetadata(const message::Message& msg, const eckit::LocalConfiguration& additionalMetadata);
    void setOceanMetadata(const message::Message& msg, const eckit::LocalConfiguration& additionalMetadata);

    void setOceanCoordMetadata(const message::Metadata& metadata, const eckit::Configuration& additionalMetadata);

    template <typename T>
    message::Message setFieldValues(const message::Message& msg);


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


}  // namespace multio::action
