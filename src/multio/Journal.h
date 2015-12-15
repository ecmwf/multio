/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @date Dec 2015


#ifndef multio_Journal_H
#define multio_Journal_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class Journal : private eckit::NonCopyable {

public:

    Journal();

    ~Journal();

protected:

    void print(std::ostream&) const;

private:

    friend std::ostream &operator<<(std::ostream &s, const Journal &p) {
        p.print(s);
        return s;
    }

private:

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif

