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
#include <sys/time.h>

#include "eckit/memory/NonCopyable.h"
#include "eckit/types/FixedString.h"
#include "eckit/io/DataHandle.h"

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
    //      > TODO: Some identifying information about the (sub-)DataSink that generated
    //              this record so that it can be sent to the correct sink for processing.
    //      > size_t length
    //      > Data (specific to the DataSink that did the journaling)
    //   -- End
    //      >> Code 'E'

    enum RecordType {
        EndOfJournal
    };
    const static unsigned char currentTagVersion = 1;

    struct Head {

        unsigned char   tag_;           /// (1)
        unsigned char   tagVersion_;    /// (1)

        char            unused_[2];     /// (2)  reserved for future use

        timeval         timestamp_;     /// (16) date & time of entry (in Unix seconds)

        char            unused2_[12];    /// (12) reserved for future use
                                         /// TOTAL: 32
    } head_;

    // Add a way to stream all the journal elements out.

    // Payload goes here (should depend on the specific type. Will be pointed to.
    // eckit::ScopedPtr<JournalRecordPayload> payload_

    eckit::FixedString<4> marker_;           /// (4) Termination marker

public: // methods

    /// Initialise a blank JournalRecord with valid values to be written to the journal log.
    void initialise(RecordType type);

    /// Write the journal record to the supplied data handle
    void writeRecord(eckit::DataHandle& handle);

};

//--------------------------------------------------------------------------------------------------

}  // namespace multio

#endif // multio_JournalRecord_H

