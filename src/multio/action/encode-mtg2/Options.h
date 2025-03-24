/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Oct 2025

#pragma once


#include <optional>
#include <string>

#include "multio/action/Action.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"


namespace multio {

namespace action {
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

using action::EncodeMtg2Def;

MULTIO_KEY_SET_DESCRIPTION(EncodeMtg2Def,                                                                       //
                           "encode-mtg2",                                                                       //
                                                                                                                //
                           KeyDef<EncodeMtg2Def::KnowledgeRoot, std::string>{"knowledge-root"}.tagDefaulted(),  //
                           KeyDef<EncodeMtg2Def::SamplesPath, std::string>{"samples-path"}.tagDefaulted(),      //
                           KeyDef<EncodeMtg2Def::EncodingRules, std::string>{"encoding-rules"}.tagDefaulted(),  //
                           KeyDef<EncodeMtg2Def::MappingRules, std::string>{"mapping-rules"}.tagDefaulted(),    //
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


namespace action {

using EncodeMtg2KeySet = datamod::KeySet<EncodeMtg2Def>;
using EncodeMtg2Conf = datamod::KeyValueSet<EncodeMtg2KeySet>;

//---------------------------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio
