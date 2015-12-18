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

#include "JournalRecord.h"

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


void JournalRecord::writeRecord(DataHandle& handle) {

    handle.write(&head_, sizeof(head_));

    // TODO: We write the payload data here.

    if (dataBuffer_) {
        
        JournalEntry entry;
        eckit::zero(entry.head_);
        entry.head_.tag_ = JournalEntry::Data;
        entry.head_.payload_length = dataBuffer_->size();

        handle.write(&entry.head_, sizeof(entry.head_));
        handle.write(*dataBuffer_, dataBuffer_->size());
    }

    handle.write(&marker_, sizeof(marker_));
}


void JournalRecord::addData(const void * data, const Length& length) {

    // TODO: We don't necessarily have to do a buffer copy here. We do for now, and
    //       async stuff will be more complicated, but  at least for blocking I/O
    //       the supplied data should outlive the journalrecord...

    if (!dataBuffer_) {
        // n.b. Don't worry about const cast. That is just to make Buffer constructor happy.
        //      The overall Buffer is const...
        dataBuffer_.reset(new Buffer(const_cast<void*>(data), length, false));
    }

    // Now that something has been added, we should certainly write this entry on exit!
    utilised_ = true;
}

// -------------------------------------------------------------------------------------------------

} // namespace multio
