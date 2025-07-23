/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once


#include "multio/datamod/core/KeySet.h"
#include "multio/datamod/core/KeyValueSet.h"

#include "multio/util/TypeToString.h"

//-----------------------------------------------------------------------------

namespace multio::util {

template <typename EnumType>
struct TypeToString<datamod::KeySet<EnumType>> {
    std::string operator()() const {
        return std::string("datamod::KeySet<") + std::string(datamod::KeySetName_v<EnumType>) + std::string(">");
    };
};

template <auto id>
struct TypeToString<datamod::KeyId<id>> {
    std::string operator()() const {
        return std::string(datamod::KeySetName_v<decltype(id)>) + std::string("::")
             + std::string(datamod::key<id>().key());
    };
};

template <auto... Ids>
struct TypeToString<datamod::CustomKeySet<Ids...>> {
    std::string operator()() const {
        return std::string("datamod::CustomKeySet<") + ((typeToString<datamod::KeyId<Ids>>() + std::string(", ")), ...)
             + std::string(">");
    };
};

template <typename KeySet_>
struct TypeToString<datamod::KeyValueSet<KeySet_>> {
    std::string operator()() const {
        return std::string("datamod::KeyValueSet<") + util::typeToString<KeySet_>() + std::string(">");
    };
};
}  // namespace multio::util

