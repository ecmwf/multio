/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

namespace multio::message {

//=============================================================================

class Metadata : public eckit::LocalConfiguration {
public:
    Metadata() = default;
    Metadata(const eckit::Configuration& config);
};

std::string to_string(const Metadata& metadata);
Metadata to_metadata(const std::string& fieldId);

//=============================================================================


class MetadataException : public eckit::Exception {
public:
    MetadataException(const std::string& reason, const eckit::CodeLocation& l = eckit::CodeLocation());
};

class MetadataMissingKeyException : public MetadataException {
public:
    MetadataMissingKeyException(const std::string& missingKey, const eckit::CodeLocation& l = eckit::CodeLocation());
};


//=============================================================================

}  // namespace multio::message
