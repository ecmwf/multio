/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#pragma once


#include <memory>
#include <optional>
#include <string>

#include "eccodes.h"
#include "metkit/codes/CodesHandleDeleter.h"
#include "multio/action/ChainedAction.h"
#include "multio/message/DataModelling.h"
#include "multio/message/Glossary.h"
#include "multiom/api/c/api.h"

namespace multio::action::encode_mtg2 {
struct ForeignDictType;
}

template <>
class std::default_delete<multio::action::encode_mtg2::ForeignDictType> {
public:
    void operator()(multio::action::encode_mtg2::ForeignDictType* ptr) const {
        void* p = static_cast<void*>(ptr);
        ASSERT(multio_grib2_dict_destroy(&p) == 0);
    }
};


namespace multio::action::encode_mtg2 {
struct ForeignEncoderType;
}

template <>
class std::default_delete<multio::action::encode_mtg2::ForeignEncoderType> {
public:
    void operator()(multio::action::encode_mtg2::ForeignEncoderType* ptr) const {
        void* p = static_cast<void*>(ptr);
        // TODO uncomment with new function
        // ASSERT(multio_grib2_encoder_close(&p) == 0);
    }
};


namespace multio::action::encode_mtg2 {

enum class MultiOMEncoderKind : unsigned long
{
    Simple,
    Cached,
};

std::string multiOMEncoderKindString(MultiOMEncoderKind kind);

enum class MultiOMDictKind : unsigned long
{
    Options,
    MARS,
    Parametrization,
    // Geometry dicts
    ReducedGG,
    RegularLL,
    SH,
};

struct EncodeMultiOMOptions {
    std::optional<std::string> knowledgeRoot;
    std::optional<std::string> samplesPath;
    std::optional<std::string> encodingFile;
    std::optional<std::string> mappingFile;
};

std::string multiOMDictKindString(MultiOMDictKind kind);

struct MultiOMDict {
    MultiOMDict(MultiOMDictKind kind);
    ~MultiOMDict() = default;

    MultiOMDict(MultiOMDict&&) noexcept = default;
    MultiOMDict& operator=(MultiOMDict&&) noexcept = default;

    void toYAML(const std::string& file = "stdout");

    void set(const char* key, const char* val);
    void set(const std::string& key, const std::string& val);

    // Typed setters
    void set(const std::string& key, std::int64_t val);
    void set(const std::string& key, double val);
    void set(const std::string& key, bool val);
    void set(const std::string& key, const std::int64_t* val, std::size_t len);
    void set(const std::string& key, const double* val, std::size_t len);
    void set(const std::string& key, const std::vector<std::int64_t>& val);
    void set(const std::string& key, const std::vector<double>& val);

    // Set geoemtry on parametrization
    void set_geometry(MultiOMDict&& geom);

    void* get();

    MultiOMDictKind kind_;
    std::unique_ptr<ForeignDictType> dict_;
    std::unique_ptr<MultiOMDict> geom_;
};

}  // namespace multio::action::encode_mtg2


namespace multio {

template <>
struct message::KeyValueWriter<action::MultiOMDict> {
    template <typename KVD, typename KV_,
              std::enable_if_t<(IsKeyValueDescription_v<std::decay_t<KVD>> && IsKeyValue_v<std::decay_t<KV_>>), bool>
              = true>
    static void set(const KVD& kvd, KV_&& kv, action::MultiOMDict& md) {
        using KV = std::decay_t<KV_>;
        if constexpr (KVD::hasMapper) {
            std::forward<KV_>(kv).visit(
                eckit::Overloaded{[&](MissingValue v) {},
                                  [&](auto&& v) { md.set(kvd.key, kvd.mapper.write(std::forward<decltype(v)>(v))); }});
        }
        else {
            std::forward<KV_>(kv).visit(eckit::Overloaded{
                [&](MissingValue v) {}, [&](auto&& v) { md.set(kvd.key, std::forward<decltype(v)>(v)); }});
        }
    }
};

}  // namespace multio


namespace multio::action {

struct MultiOMEncoder {
    MultiOMEncoder(MultiOMDict& options);

    std::unique_ptr<codes_handle> encode(MultiOMDict& mars, MultiOMDict& par, const double* data, std::size_t len);
    std::unique_ptr<codes_handle> encode(MultiOMDict& mars, MultiOMDict& par, const float* data, std::size_t len);

    ~MultiOMEncoder();

    void* encoder_ = nullptr;
};


// New encoder for caching
struct MultiOMRawEncoder {
    MultiOMRawEncoder(MultiOMEncoderKind kind, MultiOMDict& options, MultiOMDict& mars);
    ~MultiOMRawEncoder() = default;

    MultiOMRawEncoder(MultiOMRawEncoder&&) noexcept = default;
    MultiOMRawEncoder& operator=(MultiOMRawEncoder&&) noexcept = default;

    void* get();

    MultiOMEncoderKind kind_;
    std::unique_ptr<ForeignEncoderType> encoder_;
};

class EncoderCache {
public:
    EncoderCache(MultiOMEncoderKind kind, MultiOMDict&& options);
    MultiOMRawEncoder& getEncoder(const message::MarsKeyValueSet& marsKeys, MultiOMDict& marsDict);


private:
    MultiOMEncoderKind kind_;
    MultiOMDict options_;
    std::unordered_map<message::EncoderCacheMarsKeyValueSet, MultiOMRawEncoder> cache_{};
};


class EncodeMtg2 : public ChainedAction {
public:
    explicit EncodeMtg2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    // Internal constructor delegate with prepared configuration for specific
    // encoder
    explicit EncodeMtg2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encoderConf);

    void print(std::ostream& os) const override;

    EncodeMultiOMOptions options_;
    MultiOMEncoder encoder_;  // @Mirco this may be removed or used optionally
    EncoderCache cache_;
};

//---------------------------------------------------------------------------------------------------------------------

class EncodeMtg2Exception : public eckit::Exception {
public:
    EncodeMtg2Exception(const std::string& reason, const eckit::CodeLocation& location = eckit::CodeLocation());
};

//---------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action::encode_mtg2
