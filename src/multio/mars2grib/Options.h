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


#include <string>

#include "multio/datamod/DataModelling.h"

#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"


namespace multio {

// TODO rewrite with plain struct and split

namespace mars2grib {
enum class EncodeMtg2Def : std::uint64_t
{
    KnowledgeRoot,
    SamplesPath,
    EncodingRules,
    MappingRules,
    GeoFromAtlas
};
}

namespace datamod {

using mars2grib::EncodeMtg2Def;

MULTIO_KEY_SET_DESCRIPTION(
    EncodeMtg2Def,  //
    "encode-mtg2",  //
                    //
    KeyDef<EncodeMtg2Def::KnowledgeRoot, eckit::PathName>{"knowledge-root"}
        .withDefault([]() { return multio::LibMultio::instance().libraryHome(); })
        .withDescription("Path to where ./share/multiom/ is located. Default is to the library home."),  //
    KeyDef<EncodeMtg2Def::SamplesPath, eckit::PathName>{"samples-path"}                                  //
        .tagDefaulted()
        .withDescription("Path to samples. Default is <knowledge-root>/share/multiom/samples"),  //
    KeyDef<EncodeMtg2Def::EncodingRules, eckit::PathName>{"encoding-rules"}                      //
        .tagDefaulted()
        .withDescription(
            "Path to mappings file. Default is <knowledge-root/share/multiom/mappings/mapping-rules.yaml"),  //
    KeyDef<EncodeMtg2Def::MappingRules, eckit::PathName>{"mapping-rules"}                                    //
        .tagDefaulted()
        .withDescription(
            "Path to encoding file. Default is <knowledge-root/share/multiom/encodings/encodung-rules-nested.yaml"),  //
    KeyDef<EncodeMtg2Def::GeoFromAtlas, bool>{"geo-from-atlas"}.withDefault(false))


template <>
struct KeySetAlter<KeySet<EncodeMtg2Def>> {
    static void alter(KeyValueSet<KeySet<EncodeMtg2Def>>& opts) {
        using namespace datamod;
        const auto& root = key<EncodeMtg2Def::KnowledgeRoot>(opts)
                               .withDefault([]() { return multio::LibMultio::instance().libraryHome(); })
                               .get();

        key<EncodeMtg2Def::SamplesPath>(opts).withDefault([&]() { return root + "/share/multiom/samples"; });
        key<EncodeMtg2Def::MappingRules>(opts).withDefault(
            [&]() { return root + "/share/multiom/mappings/mapping-rules.yaml"; });
        key<EncodeMtg2Def::EncodingRules>(opts).withDefault(
            [&]() { return root + "/share/multiom/encodings/encoding-rules-nested.yaml"; });
    }
};

};  // namespace datamod


namespace mars2grib {

using EncodeMtg2KeySet = datamod::KeySet<EncodeMtg2Def>;
using EncodeMtg2Conf = datamod::KeyValueSet<EncodeMtg2KeySet>;

//---------------------------------------------------------------------------------------------------------------------

}  // namespace mars2grib
}  // namespace multio
