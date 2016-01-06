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
#include <sstream>

#include "eckit/config/JSONConfiguration.h"

#include "multio/Journal.h"
#include "multio/JournalReader.h"

using namespace eckit;

namespace multio {

// -------------------------------------------------------------------------------------------------

JournalReader::JournalReader(const Configuration& config, const PathName& path) : 
    Journal(config),
    path_(path),
    handle_(path.fileHandle()) {

    eckit::Log::info() << "[" << *this << "] Opening journal file: " << path_ << std::endl;

    // Read the header from the datahandle, and check that everything is valid.
    handle_->openForRead();
    handle_->read(&head_, sizeof(Journal::Header));

    eckit::Log::info() << "[" << *this << "] Journal file version: " << int(head_.tagVersion_) << std::endl;

    ASSERT(head_.tag_ == Journal::CurrentHeaderTag);
    ASSERT(head_.tagVersion_ == Journal::CurrentVersion);

    readConfiguration();
}


JournalReader::~JournalReader() {}


/// Read the next journal record.
///
/// This routine reades the next journal record (including an arbitrary number of
/// JournalEntries). If a structural error is encountered, or if the end of file
/// is reached in an inappropriate way, an exception is thrown.
///
/// @param record the JournalRecord to initialise.

bool JournalReader::readRecord(JournalRecord& record) {

    handle_->read(&record.head_, sizeof(JournalRecord::Head));

    eckit::Log::info() << "[" << *this << "] Read journal record" << std::endl;
    eckit::Log::info() << "[" << *this << "]  - Record type: "
                       << JournalRecord::RecordTypeNames[record.head_.tag_] << std::endl;
    eckit::Log::info() << "[" << *this << "]  - Num entries: "
                       << int(record.head_.numEntries_) << std::endl;

    // Given the header, allocate space to store the contained JournalEntries
    record.entries_.resize(record.head_.numEntries_);

    // Loop over the entries contained in the journal record
    int i = 0;
    for (std::list<JournalRecord::JournalEntry>::iterator it = record.entries_.begin();
                        it != record.entries_.end(); (++i, ++it) ) {

        // Read the (header) of the Journal Entry
        JournalRecord::JournalEntry& entry(*it);
        handle_->read(&entry.head_, sizeof(JournalRecord::JournalEntry::Header));
        eckit::Log::info() << "[" << *this << "]  * Read length: "
                           << sizeof(JournalRecord::JournalEntry::Header) << std::endl;

        eckit::Log::info() << "[" << *this << "]  * Read entry (" << i << ")" << std::endl;
        eckit::Log::info() << "[" << *this << "]     - entry type: "
                           << entry.head_.tag_ << std::endl << std::flush;
        eckit::Log::info() << "[" << *this << "]     - Payload length: "
                           << entry.head_.payload_length_ << std::endl << std::flush;

        // Deal with any associated data payload
        if (entry.head_.payload_length_ != 0) {
            entry.data_.reset(new SharableBuffer(entry.head_.payload_length_));
            eckit::Log::info() << "[" << *this << "]     - reading payload ("
                               << entry.data_->size() << ")" << std::endl << std::flush;
            handle_->read(*entry.data_, size_t(entry.head_.payload_length_));

            //eckit::Log::info() << "[" << *this << "]     - " << std::string(*entry.data_, size_t(entry.head_.payload_length_)) << "---" << std::endl << std::flush;
        }
        eckit::Log::info() << "[" << *this << "]     - done" << std::endl << std::flush;
    }

    eckit::Log::info() << "[" << *this << "]  - Finished reading entries" << std::endl << std::flush;

    // Check that we get the terminantion marker!
    handle_->read(record.marker_.data(), sizeof(record.marker_.size()));

    ASSERT(record.marker_ == JournalRecord::TerminationMarker);

    return true;
}


/*
 * Read the JournalRecord corresponding to the JSON configuration stored in the file.
 * This will (MUST) be the first JournalRecord in the file.
 *
 * This is NOT part of the header structure, as although we always include it in
 * version 1, we don't want to assume that this will always be the case.
 */
void JournalReader::readConfiguration() {

    JournalRecord record(*this, JournalRecord::Uninitialised);
    readRecord(record);

    ASSERT(record.head_.tag_ == JournalRecord::Configuration);
    ASSERT(record.head_.numEntries_ == 1);
    ASSERT(record.entries_.size() == 1);

    // Manipulate this into an istream for the JSON configuration.
    JournalRecord::JournalEntry& dataEntry(record.entries_.front());
    SharableBuffer& buffer(*dataEntry.data_);

    ASSERT(buffer.size() == dataEntry.head_.payload_length_);

    std::string json_str(buffer, buffer.size());

    std::istringstream iss(json_str);

    JSONConfiguration file_config(iss);

    Log::info() << "[" << *this << "]  - Journal file configuration: " << std::endl;
    Log::info() << "-------------------------------------------------------------" << std::endl;
    Log::info() << file_config.get() << std::endl;
    Log::info() << "-------------------------------------------------------------" << std::endl;

    // TODO: This configuration may be tested against the externally supplied config
}



void JournalReader::print(std::ostream& os) const {
    os << "JournalReader(file=" << path_ << ")";
}

// -------------------------------------------------------------------------------------------------

} // namespace multio
