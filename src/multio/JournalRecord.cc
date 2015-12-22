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

#include <sys/time.h>

#include "eckit/io/Buffer.h"
#include "eckit/io/DataHandle.h"

#include "multio/JournalRecord.h"
#include "multio/SharableBuffer.h"

using namespace eckit;

namespace multio {

    const FixedString<4> terminationMarker("END!");
    const unsigned char currentTagVersion = 1;

// -------------------------------------------------------------------------------------------------

/*
 * TODO: SDS. For more complicated cases, it may well be woth changing utilised_ into
 *       writeOnDestruct_, and then having the JournalRecord write itself.
 *
 * --> This will require passing in the Journal object
 * --> Will require some careful consideration of locking for the journal.
 */

JournalRecord::JournalRecord(RecordType type) :
    utilised_(false){

    if (type != JournalRecord::Uninitialised) {

        initialise(type);
    }
}


JournalRecord::~JournalRecord() {
}


/// Initialise a (new) Journal record, such that it will be valid for writing
/// (once payload data has been added as appropriate).
void JournalRecord::initialise(RecordType type) {

    eckit::zero(head_);
    head_.tag_ = type;
    head_.tagVersion_ = currentTagVersion;

    SYSCALL(::gettimeofday(&head_.timestamp_, NULL));

    marker_ = terminationMarker;
}


/// Write the journal record, consisting of three parts:
/// i)   The header
/// ii)  The JournalEntries
/// iii) The end-of-record marker
void JournalRecord::writeRecord(DataHandle& handle) {

    handle.write(&head_, sizeof(head_));

    for (std::list<JournalEntry>::const_iterator it = events_.begin(); it != events_.end(); ++it) {
        handle.write(&it->head_, sizeof(it->head_));

        // If there is data associated with the journal entry then it should be appended below
        // the header information.
        if (it->data_) {
            ASSERT(it->head_.payload_length_ == it->data_->size());
            handle.write(*(it->data_), it->data_->size());
        } else {
            ASSERT(it->head_.payload_length_ == 0);
        }
    }
    
    handle.write(&marker_, sizeof(marker_));
}


void JournalRecord::addData(const void * data, const Length& length) {

    eckit::Log::info() << "... ADD: " << events_.size() << ", " << events_.empty() << std::endl << std::flush;

    // n.b. The data must be the first thing added to the Journal Record
    if (events_.empty()) {

        eckit::Log::info() << "... Add data" << std::endl << std::flush;

        events_.push_back(JournalEntry());

        JournalEntry& entry = events_.back();

        eckit::zero(entry.head_);
        entry.head_.tag_ = JournalEntry::Data;
        entry.head_.payload_length_ = length;
        SYSCALL(::gettimeofday(&entry.head_.timestamp_, NULL));

        // n.b. Don't worry about const cast. That is just to make Buffer constructor happy.
        //      The overall Buffer is const...
        //
        // We are making the promise here that the data will outlive the journal writing
        // process.
        entry.data_.reset(new SharableBuffer(const_cast<void*>(data), length, false));

        // Now that something has been added, we should certainly write this entry on exit!
        utilised_ = true;

    } else {
        eckit::Log::info() << "... Add data (already added)" << std::endl << std::flush;

        ASSERT(utilised_);
        ASSERT(events_.front().head_.tag_ == JournalEntry::Data);
        ASSERT(events_.front().head_.payload_length_ == length);
    }
}


void JournalRecord::addJournalEntry(JournalRecord::JournalEntry::EntryType type) {

    // These are (currently) default journal entries with no attached data.
    // This needs to be extended.
    eckit::Log::info() << "Adding entry" << std::endl << std::flush;
    events_.push_back(JournalEntry());
    JournalEntry& entry = events_.back();

    eckit::zero(entry.head_);
    entry.head_.tag_ = type;
    entry.head_.payload_length_ = 0;
    SYSCALL(::gettimeofday(&entry.head_.timestamp_, NULL));
}

// -------------------------------------------------------------------------------------------------

} // namespace multio
