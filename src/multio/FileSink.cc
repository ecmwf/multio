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

#include <iosfwd>
#include <fstream>

#include "multio/FileSink.h"
#include "multio/DataSink.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Length.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/io/DataHandle.h"

using namespace eckit;

//----------------------------------------------------------------------------------------------------------------------

namespace multio {

FileSink::FileSink(const Configuration& config) :
    DataSink(config),
    isOpen_(false),
    truncate_(config.getBool("truncate", false)),
    path_( config.getString("path") ),
    handle_( path_.fileHandle(false) )
{
    // config.getBool("aio",false);
}

FileSink::~FileSink() {
    close();
}

void FileSink::open_() {

    eckit::Log::info() << "[" << *this << "]: open" << std::endl;
    eckit::AutoLock<eckit::Mutex> lock(mutex_);

    if (!isOpen_) {

        if (!truncate_) {
            handle_->openForAppend(0);
        } else {
            handle_->openForWrite(0);
        }

        isOpen_ = true;
    }
}

void FileSink::write_(const void* buffer, const Length& length, JournalRecord& journal_record) {

    eckit::Log::info() << "[" << *this << "]: write (" << length << ")" << std::endl;
    eckit::AutoLock<eckit::Mutex> lock(mutex_);

    handle_->write(buffer, length);

    record_write_journal_entry(journal_record, buffer, length);
}

void FileSink::close() {

    eckit::Log::info() << "[" << *this << "]: close" << std::endl;
    eckit::AutoLock<eckit::Mutex> lock(mutex_);

    if(isOpen_) {
        handle_->close();
        isOpen_ = false;
    }
}


void FileSink::print(std::ostream& os) const {
    os << "FileSink(path=" << path_ << ")";
}

DataSinkBuilder<FileSink> FileSinkFactorySingleton("file");

}  // namespace multiplexer


