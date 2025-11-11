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

#include "metkit/codes/api/CodesTypes.h"
#include "multio/datamod/Glossary.h"
#include "multio/message/Message.h"


#include "eckit/config/LocalConfiguration.h"

#include "metkit/codes/api/CodesAPI.h"

#include "eccodes.h"


#include <memory>
#include <variant>


namespace multio::action::encode {

namespace dm = multio::datamod;

using CodesScalarValue = std::variant<std::int64_t, double, std::string>;
using CodesOverwrites = std::vector<std::pair<std::string, CodesScalarValue>>;

class GribEncoder {
public:
    GribEncoder(std::unique_ptr<metkit::codes::CodesHandle> handle, const eckit::LocalConfiguration& config);

    template <typename T>
    void setValue(const std::string& key, std::optional<T> v) {
        if (v) {
            encoder_->set(key, *v);
        }
    };

    template <typename T>
    void setValue(const std::string& key, T v) {
        if constexpr (std::is_signed_v<T> && std::is_integral_v<T>) {
            encoder_->set(key, static_cast<std::int64_t>(v));
        }
        else {
            encoder_->set(key, v);
        }
    };

    void setMissing(const std::string& key);

    template <typename T>
    void setValue(const std::string& key, const std::vector<T>& v) {
        encoder_->set(key, v);
    };
    
    // Dirty implementation - before metkit::codes::CodesHandle
    // was used, the former implementation explicitly ignored key setting errors
    // when the key was read-only.
    // As this encoder is planned to be removed, some keys with undecided read-only behaviour
    // are set with this call.
    template <typename T>
    void trySetValue(const std::string& key, T&& val) {
        try {
            setValue(key, std::forward<std::decay_t<T>>(val));
        }
        catch (metkit::codes::CodesException& e) {
            eckit::Log::info() << "Multio legacy GribEncoder::trySetValue(" << key << ",  " << val
                               << ")  failed: " << std::endl;
        }
    }
    template <typename T>
    void setDataValues(const T* data, size_t count) {
        encoder_->set("values", metkit::codes::Span<const T>(reinterpret_cast<const T*>(data), count));
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
    const std::unique_ptr<metkit::codes::CodesHandle> template_;
    std::unique_ptr<metkit::codes::CodesHandle> encoder_;

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

    // Check if metadata has a key "nemoParam" or a category starting with "ocean"
    std::optional<std::string> category = metadata.getOpt<std::string>(dm::legacy::Category);
    const bool hasNemoParam = metadata.find(dm::legacy::NemoParam) != metadata.end();
    const bool hasCatOcean = category && (category->rfind("ocean") == 0);
    return hasNemoParam || hasCatOcean;
};


}  // namespace multio::action::encode
