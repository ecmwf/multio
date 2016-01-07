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
#include <sstream>

#include "multio/Journal.h"

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/parser/JSON.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

// Initialise static members
const eckit::FixedString<8> Journal::CurrentHeaderTag("IOJOU999");
const unsigned char Journal::CurrentVersion = 1;

//----------------------------------------------------------------------------------------------------------------------

// TODO: Generate timestamped filename?
Journal::Journal(const Configuration& config) :
    footer_(*this, JournalRecord::Uninitialised),
    configurationRecord_(*this, JournalRecord::Uninitialised),
    path_(config.getString("journalfile", "journal")),
    handle_(path_.fileHandle(false)),
    config_(config),
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

        initHeader();
        // TODO: Write header

        // And actually write out this header.
        handle_->write(&head_, sizeof(head_));

        // Get the current configuration into a string, which we can test the length of
        std::stringstream json_stream;
        JSON config_json(json_stream);
        config_json << config_.get();
        std::string json_str = json_stream.str();

        configurationRecord_.initialise(JournalRecord::Configuration);
        configurationRecord_.addData(json_str.c_str(), json_str.length());
        configurationRecord_.writeRecord(*handle_);
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


void Journal::writeRecord(JournalRecord& record) {

    if(record.utilised()) {
        eckit::AutoLock<eckit::Mutex> lock(mutex_);
        record.writeRecord(*handle_);
    }
}


/// Initialise a journal header struct with valid information for writing out
void Journal::initHeader() {

    eckit::zero(head_);
    head_.tag_ = Journal::CurrentHeaderTag;
    head_.tagVersion_ = Journal::CurrentVersion;

    SYSCALL(::gettimeofday(&head_.timestamp_, NULL));

}


// TODO: make this no-longer a REALLY NAIVE routine, that just dumps the data to a
//       journal, without adding actions.
//
// i)   We ought to make the journal aware of what data we are considering. It should store a pointer.
// ii)  The (potentially various) sinks should add their journaling elements
// iii) When it is done, it should write if, and only if, there are any (non-data) elements.

void Journal::print(std::ostream& os) const
{
    os << "Journal()";
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

