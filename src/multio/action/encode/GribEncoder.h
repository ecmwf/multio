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

#include <variant>


namespace multio::action {

using CodesScalarValue = std::variant<std::int64_t, double, std::string>;
using CodesOverwrites = std::vector<std::pair<std::string, CodesScalarValue>>;
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

    message::Message encodeOceanCoordinates(message::Message&& msg, const message::Metadata& additionalMetadata);

    message::Message encodeField(message::Message&& msg, const CodesOverwrites& overwrites,
                                 const message::Metadata& additionalMetadata);

    // TODO May be refactored
    // int getBitsPerValue(int paramid, const std::string& levtype, double min, double max);

    void print(std::ostream& os) const;

private:
    // Encoder is now a member of the action
    const MioGribHandle template_;
    std::unique_ptr<MioGribHandle> encoder_;

    void initEncoder();

    void setFieldMetadata(message::Metadata& md);
    void setOceanMetadata(message::Metadata& md);

    void setOceanCoordMetadata(message::Metadata& md);

    template <typename T>
    message::Message setFieldValues(message::Message&& msg);


    const eckit::LocalConfiguration config_;

    const std::set<std::string> coordSet_{"lat_T", "lon_T", "lat_U", "lon_U", "lat_V",
                                          "lon_V", "lat_W", "lon_W", "lat_F", "lon_F"};

    // TODO: This is just included from old interface now and may require refactoring in terms of configuration and its
    // action EncodeBitsPerValue encodeBitsPerValue_;
};

inline bool isOcean(const message::Metadata& metadata) {
    using message::glossary;

    // Check if metadata has a key "nemoParam" or a category starting with "ocean"
    std::optional<std::string> category = metadata.getOpt<std::string>(glossary().category);
    const bool hasNemoParam = metadata.find(glossary().nemoParam) != metadata.end();
    const bool hasCatOcean = category && (category->rfind("ocean") == 0);
    return hasNemoParam || hasCatOcean;
};


}  // namespace multio::action
