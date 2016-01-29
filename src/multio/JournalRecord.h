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

#include "eckit/io/DataBlob.h"
#include "eckit/io/DataHandle.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/memory/ScopedPtr.h"
#include "eckit/memory/SharedPtr.h"
#include "eckit/types/FixedString.h"

#include "multio/SharableBuffer.h"

// -------------------------------------------------------------------------------------------------

namespace eckit {
    class Value;
}

// -------------------------------------------------------------------------------------------------

namespace multio {

// -------------------------------------------------------------------------------------------------

class Journal;

class JournalRecord : public eckit::OwnedLock {

public: // Data types, and structural members.

    // The journal has a certain file structure
    //
    // header
    //   -- Magic tag
    //   -- Version
    //   -- Timestamp
    // journal entries (repeated)
    //   -- Identifying tag and version
    //   -- The number of contained entries
    //   -- A creation timestamp
    //   -- Each contained entries has a single character code.
    //     -- data
    //        > Code "D"
    //        > size_t length
    //        > Data of that length
    //     -- Journal entry
    //        > Code "J"
    //        > id of the DataSink (in a MultIO)
    //        > size_t length
    //        > Data (specific to the DataSink that did the journaling)
    //     -- End
    //        >> Code 'E'
    //   -- A termination marker for the record

    // ---------------------------------------------------------

    enum RecordType {   // n.b. if we add types, update the ASSERT in JournalRecord::RecordTypeName
        Uninitialised,
        EndOfJournal,
        WriteEntry,
        Configuration
    };

    struct Head {

        unsigned char   tag_;           /// (1)
        unsigned char   tagVersion_;    /// (1)

        uint16_t        numEntries_;    /// (2) The number of JournalEntries contaitned

        timeval         timestamp_;     /// (16) date & time of entry (in Unix seconds)

        char            unused2_[12];    /// (12) reserved for future use
                                         /// TOTAL: 32
    } head_;

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

            unsigned char     tag_;             /// (1) - The EntryType
            uint16_t          id_;              /// (2) - The index of the DataSink (in a MultiIO)
            unsigned char     unused_[1];       /// (1)

            timeval           timestamp_;       /// (16)

            uint64_t          payload_length_;  /// (8)

            char              unused2_[12];

        } head_;

        // (Optional) additional data.
        // It would be nicer to have a ScopetPtr, but no rvalue-refs...
        eckit::DataBlobPtr data_;
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

    /// Add an eckit::Value containing a configuration to be serialised
    void addConfiguration(const eckit::Value& configValue);

    ///
    void addWriteEntry(const eckit::DataBlobPtr&, int sinkId);

    /// Write the journal record to the supplied data handle
    void writeRecord(eckit::DataHandle& handle);

    /// add data to the journal element
    void addData(const eckit::DataBlobPtr&);

    /// Add a new journal entry to the list
    void addJournalEntry(JournalEntry::EntryType type, int sinkId);

    bool utilised() const { return utilised_; }

    static const std::string& RecordTypeName(RecordType type);

    static const char * EntryTypeName(JournalEntry::EntryType type);

    static const char * blobTypeName(RecordType type);


protected: // methods

    void print(std::ostream&) const;

private: // methods

    void initHeader();

    friend std::ostream &operator<<(std::ostream &s, const JournalRecord &p) {
        p.print(s);
        return s;
    }

private: // internal control elements

    eckit::Mutex mutex_;

    Journal& journal_;

    bool utilised_;
    bool written_;

};

//--------------------------------------------------------------------------------------------------

typedef eckit::SharedPtr<JournalRecord> JournalRecordPtr;

//--------------------------------------------------------------------------------------------------

}  // namespace multio

#endif // multio_JournalRecord_H

