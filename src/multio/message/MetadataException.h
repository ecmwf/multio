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

#include "eckit/exception/Exceptions.h"

#include <string>

namespace multio::message {

//-----------------------------------------------------------------------------

class MetadataException : public eckit::Exception {
public:
    MetadataException(const std::string& reason, const eckit::CodeLocation& l = eckit::CodeLocation());
};

class MetadataKeyException : public MetadataException {
public:
    MetadataKeyException(const std::string& key, const std::string& more,
                         const eckit::CodeLocation& l = eckit::CodeLocation());
};

class MetadataMissingKeyException : public MetadataKeyException {
public:
    MetadataMissingKeyException(const std::string& missingKey, const eckit::CodeLocation& l = eckit::CodeLocation());
};

class MetadataWrongTypeException : public MetadataException {
public:
    MetadataWrongTypeException(const std::string& key, const eckit::CodeLocation& l = eckit::CodeLocation());
    MetadataWrongTypeException(std::size_t requestedIndex, std::size_t containedIndex,
                               const eckit::CodeLocation& l = eckit::CodeLocation());
    MetadataWrongTypeException(const eckit::CodeLocation& l = eckit::CodeLocation());
};


//-----------------------------------------------------------------------------

}  // namespace multio::message
