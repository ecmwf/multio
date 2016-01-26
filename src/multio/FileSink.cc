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
#include "eckit/io/DataHandle.h"
#include "eckit/io/Length.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"

using namespace eckit;

//----------------------------------------------------------------------------------------------------------------------

namespace multio {

FileSink::FileSink(const Configuration& config) :
    DataSink(config),
    path_( config.getString("path") ),
    handle_( path_.fileHandle(false) )
{
    handle_->openForWrite(0);
}

FileSink::~FileSink() {
    handle_->close();
}

void FileSink::write(eckit::DataBlobPtr blob, SharedPtr<JournalRecord> record) {

    size_t length = blob->length();

    eckit::Log::info() << "[" << *this << "]: write (" << length << ")" << std::endl;
    eckit::AutoLock<eckit::Mutex> lock(mutex_);

    long written = handle_->write(blob->buffer(), length);

    // We should throw an exception if we are not journaling anything
    if (record && (written != length || journalAlways_))
        record->addWriteEntry(blob, id_);
    else if (written != length)
        throw WriteError(std::string("Write error on file: ") + path_, Here());
}


void FileSink::print(std::ostream& os) const {
    os << "FileSink(path=" << path_ << ")";
}

static DataSinkBuilder<FileSink> FileSinkFactorySingleton("file");

}  // namespace multio


