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

class SharedPayloadException : public eckit::Exception {
public:
    SharedPayloadException(const std::string& reason, const eckit::CodeLocation& l = eckit::CodeLocation());
};

class PayloadNotWritableException : public SharedPayloadException {
public:
    PayloadNotWritableException(const eckit::CodeLocation& l = eckit::CodeLocation());
};

//-----------------------------------------------------------------------------

}  // namespace multio::message
