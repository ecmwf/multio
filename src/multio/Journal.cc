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
/// @author Simon Smart
/// @date Dec 2015

#include <sys/time.h>

#include "multio/Journal.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"

using namespace eckit;

namespace multio {

//-------------------------------------------------------------------------------------------------


static const FixedString<8> journalHeaderTag("IOJOU999");
static const unsigned char journalVersion = 1;


//-------------------------------------------------------------------------------------------------

// TODO: Generate timestamped filename?
Journal::Journal(const Configuration& config) :
    path_(config.getString("journalfile", "journal")),
    handle_(path_.fileHandle(true)), // overwrite = 0 with length = 0 --> NEVER TRUNCATE.
    isOpen_(false) {
}

Journal::~Journal() {

    close();
}


/// Open a new journal file, and initialise it with header information
void Journal::open() {

    eckit::Log::info() << "Opening the journal (for writing)" << std::endl;
    eckit::AutoLock<eckit::Mutex> lock(mutex_);

    if (!isOpen_){
        handle_->openForWrite(0);
        isOpen_ = true;

        init_header();
        // TODO: Write header

        // And actually write out this header.
        handle_->write(&head_, sizeof(head_));
    }
}


// TODO: Close journal. Write footer.
// TODO: Ensure that all pending writes are flushed through before closing the journal
//
/// Finalise the journal by writing termination markers,
/// and close the associated file.
void Journal::close() {

    eckit::Log::info() << "Closing the journal" << std::endl;
    eckit::AutoLock<eckit::Mutex> lock(mutex_);

    if (isOpen_) {
        // TODO: do we want to write the number of records to the header?
        // Initialise and write the footer information
        // TODO: Refactor this somewhere sensible.
        footer_.initialise(JournalRecord::EndOfJournal);
        footer_.writeRecord(*handle_);
            
        handle_->close();
        isOpen_ = false;
    }
}


/// Initialise a journal header struct with valid information for writing out
void Journal::init_header() {

    head_.tag_ = journalHeaderTag;
    head_.tagVersion_ = journalVersion;

    SYSCALL(::gettimeofday(&head_.timestamp_, NULL));

}

void Journal::print(std::ostream& os) const
{
    os << "Journal()";
}



//-------------------------------------------------------------------------------------------------

}  // namespace multio

