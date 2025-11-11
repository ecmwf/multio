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


#include <memory>
#include <string>

#include "metkit/codes/api/CodesAPI.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/types/TypeOfProcessedData.h"
#include "multio/util/VariantHelpers.h"
#include "multiom/api/c/api.h"

namespace multio::mars2grib {
struct ForeignDictType;
}

template <>
class std::default_delete<multio::mars2grib::ForeignDictType> {
public:
    void operator()(multio::mars2grib::ForeignDictType* ptr) const {
        void* p = static_cast<void*>(ptr);
        ASSERT(multio_grib2_dict_destroy(&p) == 0);
    }
};


namespace multio::mars2grib {


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


    MultIOMDictKind kind_;
    std::unique_ptr<ForeignDictType> dict_;
    std::unique_ptr<MultIOMDict> geom_;
};

}  // namespace multio::mars2grib


namespace multio::datamod {

template <>
struct DumpType<TypeOfProcessedData, mars2grib::MultIOMDict> {
    static int64_t dump(TypeOfProcessedData d) {
        return DumpType<TypeOfProcessedData, metkit::codes::CodesHandle>::dump(d);
    };
};

template <>
struct EntryDumper<mars2grib::MultIOMDict> {
    template <
        typename EntryDef_, typename Entry_,
        std::enable_if_t<(IsBaseEntryDefinition_v<std::decay_t<EntryDef_>> && IsEntry_v<std::decay_t<Entry_>>), bool>
        = true>
    static void set(const EntryDef_& entryDef, Entry_&& entry, mars2grib::MultIOMDict& md, const DumpOptions& opts) {
        using TP = typename EntryDef_::ParserDumper;
        std::forward<Entry_>(entry).visit(          //
            eckit::Overloaded{[&](UnsetType v) {},  // Set nothing...
                              [&](auto&& v) {
                                  TP::template dumpToAndVisit<mars2grib::MultIOMDict>(
                                      std::forward<decltype(v)>(v), [&](auto&& vi) {
                                          md.set(std::string(entryDef.key()), std::forward<decltype(vi)>(vi));
                                      });
                              }});
    }
};

}  // namespace multio::datamod

