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


#include "multio/message/details/Glossary.h"
#include "multio/message/details/Metadata.h"
#include "multio/message/details/MetadataFromValue.h"


namespace multio::message {

//-----------------------------------------------------------------------------

using MetadataTraits = details::DefaultMetadataTraits;

using MetadataValue = details::MetadataValue<MetadataTraits>;
using Metadata = details::Metadata<MetadataTraits>;

using MetadataTypes = details::MetadataTypes<MetadataTraits>;

using Glossary = details::Glossary<MetadataTraits>;

using Null = details::Null;


std::string toString(const Metadata& metadata);

Metadata toMetadata(const eckit::Value& value);

Metadata toMetadata(const std::string& fieldId);

std::optional<MetadataValue> toMetadataValue(const eckit::Value& v);

//-----------------------------------------------------------------------------

}  // namespace multio::message
