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
/// @date   Dec 2015


#ifndef multio_grib_GribPacking_H
#define multio_grib_GribPacking_H

#include <iosfwd>

#include "eckit/memory/NonCopyable.h"
#include "eckit/config/Configuration.h"

namespace multio {
namespace grib {

//----------------------------------------------------------------------------------------------------------------------

class GribPacking : private eckit::NonCopyable {
public:

    explicit GribPacking(const eckit::Configuration& config);

    int bitsPerValue() const;

public:

    void print(std::ostream&) const;

    friend std::ostream& operator<<(std::ostream& s, const GribPacking& p) {
        p.print(s);
        return s;
    }
};

}  // namespace grib
}  // namespace multio

//----------------------------------------------------------------------------------------------------------------------

#endif
