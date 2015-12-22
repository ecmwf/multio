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

#include "eckit/config/Configuration.h"

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
}


JournalReader::~JournalReader() {}


/// Read the next journal record. If we have reached the end, then report it. If something is wrong,
/// then we need to throw.
/// ...


void JournalReader::print(std::ostream& os) const {
    os << "JournalReader(file=" << path_ << ")";
}

// -------------------------------------------------------------------------------------------------

} // namespace multio
