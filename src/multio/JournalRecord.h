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
#include <stdint.h>

#include "eckit/io/Buffer.h"
#include "eckit/io/DataHandle.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/memory/ScopedPtr.h"
#include "eckit/memory/SharedPtr.h"
#include "eckit/types/FixedString.h"

#include "multio/SharableBuffer.h"

namespace multio {

//-------------------------------------------------------------------------------------------------

class Journal;

class JournalRecord {

public: // Data types, and structural members.

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

    // ---------------------------------------------------------

    enum RecordType {
        Uninitialised,
        EndOfJournal,
        WriteEntry
    };

    struct Head {

        unsigned char   tag_;           /// (1)
        unsigned char   tagVersion_;    /// (1)

        uint16_t        numEntries_;    /// (2) The number of JournalEntries contaitned

        timeval         timestamp_;     /// (16) date & time of entry (in Unix seconds)

        char            unused2_[12];    /// (12) reserved for future use
                                         /// TOTAL: 32
    } head_;


    const static std::string RecordTypeNames[];

    const static eckit::FixedString<4> TerminationMarker;

    const static unsigned char CurrentTagVersion;


    // ---------------------------------------------------------

    // Inside the JournalRecord, there are a series of JournalEntries.
    // Each of these starts with a (very brief) header, describing what it is.

    struct JournalEntry {

        enum EntryType {
            Data = 'D',
            Write = 'W',
            End = 'E'
        };

        struct Header {

            // TODO: What other information needs to go into here?

            unsigned char     tag_;             /// (1)
            unsigned char     unused_[3];       /// (3)

            timeval           timestamp_;       /// (16)

            uint64_t          payload_length_;  /// (8)

            char              unused2_[12];

        } head_;

        // (Optional) additional data.
        // It would be nicer to have a ScopetPtr, but no rvalue-refs...
        eckit::SharedPtr<SharableBuffer> data_;
    };

    std::list<JournalEntry> entries_;

    // Add a way to stream all the journal elements out.

    // Payload goes here (should depend on the specific type. Will be pointed to.
    // eckit::ScopedPtr<JournalRecordPayload> payload_
    // ---------------------------------------------------------

    eckit::FixedString<4> marker_;           /// (4) Termination marker

public: // methods

    /// Create a (blank) journal record associated with a given journal. If
    /// triggered this can then 
    JournalRecord(Journal& journal, RecordType type=Uninitialised);

    ~JournalRecord();

    /// Initialise a blank JournalRecord with valid values to be written to the journal log.
    void initialise(RecordType type);

    ///
    void write(const void * data, const eckit::Length& length);

    /// Write the journal record to the supplied data handle
    void writeRecord(eckit::DataHandle& handle);

    /// add data to the journal element
    void addData(const void * data, const eckit::Length& length);

    /// Add a new journal entry to the list
    /// TODO: Add a way to attach data...
    void addJournalEntry(JournalEntry::EntryType type);

    bool utilised() const { return utilised_; }

private: // internal control elements

    Journal& journal_;

    bool utilised_;

};

//--------------------------------------------------------------------------------------------------

}  // namespace multio

#endif // multio_JournalRecord_H

