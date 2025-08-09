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


#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Print.h"
#include "multio/datamod/core/Record.h"

#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"


namespace multio {

// TODO split MULTIOM args from action args

namespace mars2grib {

namespace dm = multio::datamod;


constexpr auto KnowledgeRoot
    = dm::EntryDef<eckit::PathName>{"knowledge-root"}  //
          .withDefault([]() { return multio::LibMultio::instance().libraryHome(); })
          .withDescription("Path to where ./share/multiom/ is located. Default is to the library home.")
          .withAccessor([](auto&& v) { return &v.knowledgeRoot; });
constexpr auto SamplesPath = dm::EntryDef<eckit::PathName>{"samples-path"}
                                 .tagDefaulted()
                                 .withDescription("Path to samples. Default is <knowledge-root>/share/multiom/samples")
                                 .withAccessor([](auto&& v) { return &v.samplesPath; });
constexpr auto EncodingRules
    = dm::EntryDef<eckit::PathName>{"encoding-rules"}  //
          .tagDefaulted()
          .withDescription(
              "Path to mappings file. Default is <knowledge-root/share/multiom/mappings/mapping-rules.yaml")
          .withAccessor([](auto&& v) { return &v.encodingRules; });
constexpr auto MappingRules
    = dm::EntryDef<eckit::PathName>{"mapping-rules"}  //
          .tagDefaulted()
          .withDescription(
              "Path to encoding file. Default is <knowledge-root/share/multiom/encodings/encodung-rules-nested.yaml")
          .withAccessor([](auto&& v) { return &v.mappingRules; });
constexpr auto GeoFromAtlas
    = dm::EntryDef<bool>{"geo-from-atlas"}.withDefault(false).withAccessor([](auto&& v) { return &v.geoFromAtlas; });


struct EncodeMtg2Conf {
    dm::EntryType_t<decltype(KnowledgeRoot)> knowledgeRoot;
    dm::EntryType_t<decltype(SamplesPath)> samplesPath;
    dm::EntryType_t<decltype(EncodingRules)> encodingRules;
    dm::EntryType_t<decltype(MappingRules)> mappingRules;
    dm::EntryType_t<decltype(GeoFromAtlas)> geoFromAtlas;

    static constexpr std::string_view record_name_ = "encode-mtg2";
    static constexpr auto record_entries_
        = std::make_tuple(KnowledgeRoot, SamplesPath, EncodingRules, MappingRules, GeoFromAtlas);
};


}  // namespace mars2grib

namespace datamod {

template <>
struct ApplyRecordDefaults<mars2grib::EncodeMtg2Conf> {
    static void applyDefaults(mars2grib::EncodeMtg2Conf& opts) {
        const auto& root = opts.knowledgeRoot  //
                               .ensureInit([]() { return multio::LibMultio::instance().libraryHome(); })
                               .get();

        opts.samplesPath.ensureInit([&]() { return root + "/share/multiom/samples"; });
        opts.mappingRules.ensureInit([&]() { return root + "/share/multiom/mappings/mapping-rules.yaml"; });
        opts.encodingRules.ensureInit([&]() { return root + "/share/multiom/encodings/encoding-rules-nested.yaml"; });
    }
};
}  // namespace datamod


namespace util {
template <>
struct Print<mars2grib::EncodeMtg2Conf> : multio::datamod::PrintRecord {};
}  // namespace util

}  // namespace multio
