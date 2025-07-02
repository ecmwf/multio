/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once


#include <memory>
#include <string>

#include "multio/action/encode-mtg2/Options.h"
#include "multio/datamod/DataModelling.h"
#include "multiom/api/c/api.h"

// extern "C" {
//     int dict_init(void** dict, const char* type, int len);
//     int dict_free(void** dict);

//     int dict_set_string(const char*, int, const char*, int);
//     int dict_set_long(const char*, int, int64_t);
//     int dict_set_double(const char*, int, double);
//     int dict_set_string_array(const char*, int, const char**, int*, int);
//     int dict_set_long_array(const char*, int, int64_t*, int);
//     int dict_set_double_array(const char*, int, double*, int);
//     int dict_has(const char*, int);

//     int dict_get_string(const char*, int, char**, int*);
//     int dict_get_long(const char*, int, int64_t*);
//     int dict_get_double(const char*, int, double*);
//     int dict_get_string_array(const char*, int, char***, int**, int*);
//     int dict_get_long_array(const char*, int, int64_t**, int*);
//     int dict_get_double_array(const char*, int, double**, int*);

//     int dict_iterator_next(void*, void**, char**, int*, const char**, int*);
//     int dict_iterator_free(void**);
// }

namespace multio::action {
struct ForeignDictType;
}

template <>
class std::default_delete<multio::action::ForeignDictType> {
public:
    void operator()(multio::action::ForeignDictType* ptr) const {
        void* p = static_cast<void*>(ptr);
        ASSERT(multio_grib2_dict_destroy(&p) == 0);
    }
};


namespace multio::action {


enum class MultIOMDictKind : unsigned long
{
    Options,
    MARS,
    Parametrization,
    // Geometry dicts
    ReducedGG,
    RegularLL,
    SH,
    HEALPix,
};

std::string multIOMDictKindString(MultIOMDictKind kind);

struct MultIOMDict {
    MultIOMDict(MultIOMDictKind kind);
    ~MultIOMDict() = default;

    MultIOMDict(MultIOMDict&&) noexcept = default;
    MultIOMDict& operator=(MultIOMDict&&) noexcept = default;

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
    
    std::string toJSON() const;

    // Set geoemtry on parametrization
    [[deprecated]]
    void set_geometry(MultIOMDict&& geom);

    void* get() const;

    static MultIOMDict makeOptions(const EncodeMtg2Conf& opts);


    MultIOMDictKind kind_;
    std::unique_ptr<ForeignDictType> dict_;
    std::unique_ptr<MultIOMDict> geom_;
};

}  // namespace multio::action


namespace multio {

template <>
struct datamod::KeyValueWriter<action::MultIOMDict> : BaseKeyValueWriter<action::MultIOMDict> {
    using Base = BaseKeyValueWriter<action::MultIOMDict>;
    using Base::set;

    template <typename KVD, typename KV_,
              std::enable_if_t<(IsDynamicKey_v<std::decay_t<KVD>> && IsBaseKeyValue_v<std::decay_t<KV_>>), bool> = true>
    static void set(const KVD& kvd, KV_&& kv, action::MultIOMDict& md) {
        using KV = std::decay_t<KV_>;
        using RW = typename KVD::ReadWrite;
        std::forward<KV_>(kv).visit(eckit::Overloaded{
            [&](MissingValue v) {},
            [&](auto&& v) {
                md.set(kvd.key(), RW::template write<action::MultIOMDict>(std::forward<decltype(v)>(v)));
            }});
    }
};

}  // namespace multio

