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


#include "eckit/exception/Exceptions.h"


namespace multio::action {

//---------------------------------------------------------------------------------------------------------------------

class EncodeMtg2Exception : public eckit::Exception {
public:
    EncodeMtg2Exception(const std::string& reason, const eckit::CodeLocation& location = eckit::CodeLocation());
};

//---------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action
