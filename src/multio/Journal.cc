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

#include "multio/DataSink.h"
#include "multio/Journal.h"

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/DataBlob.h"
#include "eckit/log/Log.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

// Initialise static members
const FixedString<8> Journal::CurrentHeaderTag("IOJOU999");
const unsigned char Journal::CurrentVersion = 1;

//----------------------------------------------------------------------------------------------------------------------

Journal::Journal(const Configuration& config, DataSink * const dataSink) :
    configurationRecord_(*this, JournalRecord::Uninitialised),
    footer_(*this, JournalRecord::Uninitialised),
    path_(config.getString("journalfile", "journal")),
    handle_(path_.fileHandle(false)),
    dataSink_(dataSink),
    config_(config),
    isOpen_(false) {
}

Journal::~Journal() {
    close();
}


/// Open a new journal file, and initialise it with header information
void Journal::open() {

    Log::info() << "[" << *this << "] Opening the journal (for writing)" << std::endl;
    AutoLock<Mutex> lock(mutex_);

    if (!isOpen_){

        if (NULL == dataSink_) {
            throw SeriousBug("Can only open a journal for writing if it is associated with a MultIO DataSink",
                             Here());
        }

        handle_->openForWrite(0);
        isOpen_ = true;

        initHeader();

        // And actually write out this header.
        handle_->write(&head_, sizeof(head_));

        // Write out the configuration information associated with the
        configurationRecord_.initialise(JournalRecord::Configuration);
        configurationRecord_.addConfiguration(dataSink_->configValue());
        configurationRecord_.writeRecord(*handle_);
    }
}


// TODO: Ensure that all pending writes are flushed through before closing the journal
//
/// Finalise the journal by writing termination markers,
/// and close the associated file.
void Journal::close() {

    AutoLock<Mutex> lock(mutex_);

    if (isOpen_) {
        Log::info() << "[" << *this << "] Closing the journal" << std::endl;

        // TODO: do we want to write the number of records to the header?
        // Initialise and write the footer information
        footer_.initialise(JournalRecord::EndOfJournal);
        footer_.writeRecord(*handle_);
            
        handle_->close();
        isOpen_ = false;
    }
}


void Journal::writeRecord(JournalRecord& record) {

    if(record.utilised()) {
        AutoLock<Mutex> lock(mutex_);
        record.writeRecord(*handle_);
    }
}


/// Initialise a journal header struct with valid information for writing out
void Journal::initHeader() {

    zero(head_);
    head_.tag_ = Journal::CurrentHeaderTag;
    head_.tagVersion_ = Journal::CurrentVersion;

    SYSCALL(::gettimeofday(&head_.timestamp_, NULL));

}


bool Journal::isOpen() const {
    return isOpen_;
}


void Journal::print(std::ostream& os) const
{
    os << "Journal()";
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

