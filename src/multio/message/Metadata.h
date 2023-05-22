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

namespace multio {
namespace message {

class Metadata : public eckit::LocalConfiguration {
public:
    Metadata() = default;
    Metadata(const eckit::Configuration& config);
};

std::string to_string(const Metadata& metadata);
Metadata to_metadata(const std::string& fieldId);

}  // namespace message
}  // namespace multio
