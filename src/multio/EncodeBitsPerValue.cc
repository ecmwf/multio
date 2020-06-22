/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @date June 2020

#include <fstream>
#include <iosfwd>

#include "multio/DataSink.h"
#include "multio/EncodeBitsPerValue.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/DataHandle.h"

using namespace eckit;

//----------------------------------------------------------------------------------------------------------------------

namespace multio {

EncodeBitsPerValue::EncodeBitsPerValue(const Configuration& config) {
}

int EncodeBitsPerValue::findBitsPerValue(int paramid, const std::string& levtype) {
  return 16;
}

}  // namespace multio
