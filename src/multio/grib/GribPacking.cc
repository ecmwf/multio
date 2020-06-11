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
/// @author Domokos Sarmany
/// @date   June 2020

#include "multio/grib/GribPacking.h"

#include "eckit/log/Log.h"

#include "multio/LibMultio.h"

namespace multio {
namespace grib {

GribPacking::GribPacking(const eckit::Configuration& config) {}

void GribPacking::print(std::ostream& os) const {
    os << "GribPacking()";
}

}  // namespace grib
}  // namespace multio
