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

#pragma once

#include "multio/message/BaseMetadata.h"


//----------------------------------------------------------------------------------------------------------------------


namespace multio::message {

class Metadata : public BaseMetadata {
public:
    using Base = BaseMetadata;
    using This = Metadata;
    using KeyType = typename MetadataTypes::KeyType;
    using MapType = typename MetadataTypes::MapType<MetadataValue>;

    virtual ~Metadata() {};

    using BaseMetadata::BaseMetadata;

    Metadata(const BaseMetadata& b) : BaseMetadata(b) {}
    BaseMetadata toBaseMetadata() const { return BaseMetadata{values_}; }
    operator std::unique_ptr<BaseMetadata>() { return std::make_unique<BaseMetadata>(values_); }

    using Base::operator=;

    using Base::get;
    using Base::getOpt;


    using Base::operator[];

    using Base::set;
    using Base::trySet;

    using Iterator = typename MapType::iterator;
    using ConstIterator = typename MapType::const_iterator;

    // The single place we need to change to support lookups in parametrization.
    // Important: The non-const version will copy values from the global parametrization dictionary
    //            to the local one - It will always return an iterator to the local dictionary.
    //            This allows values to be modified in the local object directly.
    //
    //            The const version will return either an iterator to the local object or the
    //            global parametrization dictionary.
    //            If keys are assumed to hold nested objects or large arrays which won't be modified anyway -
    //            it is reasonable to explicitly const-cast the object to select the const lookup version.
    Iterator find(const KeyType& k) override;
    ConstIterator find(const KeyType& k) const override;


    using Base::begin;
    using Base::cbegin;
    using Base::cend;
    using Base::end;

    using Base::erase;

    using Base::empty;

    using Base::size;

    using Base::clear;

    using Base::merge;

    using Base::updateNoOverwrite;
    using Base::updateOverwrite;

    using Base::json;
    using Base::toString;
};


//----------------------------------------------------------------------------------------------------------------------


Metadata metadataFromYAML(const std::string& yamlString);

Metadata toMetadata(const eckit::Value& value);

Metadata toMetadata(const eckit::Configuration& value);


//-----------------------------------------------------------------------------


}  // namespace multio::message
