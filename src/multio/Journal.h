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

class JournalMessage {

    // The journal has a certain file structure
    //
    // header
    //   -- Magic tag
    //   -- Version
    // journal entries (repeated)
    //   -- Each contained element has a single character code.
    //   -- data
    //      > Code "D"
    //      > size_t length
    //      > Data of that length
    //   -- Journal entry
    //      > Code "J"
    //      > TODO: Some identifying information about the 
    //      > size_t length
    //      > Data (specific to the DataSink that did the journaling)

};

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

