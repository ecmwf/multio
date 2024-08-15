/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Sept 2023

#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"

#include "eckit/parser/YAMLParser.h"

#include <sstream>


namespace multio::message {


//----------------------------------------------------------------------------------------------------------------------

typename Metadata::Iterator Metadata::find(const KeyType& k) {
    auto localIt = values_.find(k);
    if (localIt != values_.end()) {
        return localIt;
    }

    const auto& global = Parametrization::instance().get();
    auto globalIt = global.find(k);
    if (globalIt != values_.end()) {
        return values_.insert(*globalIt).first;
    }

    // Nothing is found - make sure an proper iterator is returned to allow users to have a proper comparison with
    // end().
    return localIt;
};

typename Metadata::ConstIterator Metadata::find(const KeyType& k) const {
    auto localIt = values_.find(k);
    if (localIt != values_.end()) {
        return localIt;
    }

    const auto& global = Parametrization::instance().get();
    auto globalIt = global.find(k);
    if (globalIt != values_.end()) {
        // TODO - returning an iterator of a different class might be critical here?
        return globalIt;
    }

    // Nothing is found - make sure an proper iterator is returned to allow users to have a proper comparison with
    // end().
    return localIt;
};


//----------------------------------------------------------------------------------------------------------------------


Metadata metadataFromYAML(const std::string& yamlString) {
    std::istringstream in(yamlString);
    eckit::YAMLParser parser(in);
    return toMetadata(parser.parse());
}

//----------------------------------------------------------------------------------------------------------------------


Metadata toMetadata(const eckit::Value& v) {
    if (!v.isMap()) {
        std::ostringstream oss;
        oss << "toMetadata():: eckit::Value is not a map: " << v;
        throw MetadataException(oss.str(), Here());
    }
    Metadata m;

    eckit::Value keys = v.keys();
    for (unsigned int i = 0; i < keys.size(); ++i) {
        std::string key = keys[i];
        auto mv = tryToMetadataValue(v[key]);
        if (mv) {
            m.set(key, *mv);
        }
    }

    return m;
}


//----------------------------------------------------------------------------------------------------------------------

Metadata toMetadata(const eckit::Configuration& v) {
    Metadata m;

    auto keys = v.keys();
    for (unsigned int i = 0; i < keys.size(); ++i) {
        auto mv = tryToMetadataValue(v, keys[i]);
        if (mv) {
            m.set(keys[i], *mv);
        }
    }

    return m;
}


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::message
