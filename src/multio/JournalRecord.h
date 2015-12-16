/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Simon Smart
/// @date Dec 2015


#ifndef multio_JournalRecord_H
#define multio_JournalRecord_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/types/FixedString.h"

namespace multio {

//-------------------------------------------------------------------------------------------------

struct JournalRecord {

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
    //   -- End
    //      >> Code 'E'

    struct Head {

        unsigned char   tag_;
        unsigned char   tagVersion_;

        timeval         timestamp_;     /// (16) date & time of entry (in Unix seconds)

    }

    // Payload goes here (should depend on the specific type. Will be pointed to.
    // eckit::ScopedPtr<JournalRecordPayload> payload_

    eckit::FixedString<2> marker_[2];           /// (2) Termination marker

};

//--------------------------------------------------------------------------------------------------

}  // namespace multio

#endif // multio_JournalRecord_H

