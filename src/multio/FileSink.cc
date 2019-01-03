/*
 * (C) Copyright 1996- ECMWF.
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

#include <fstream>
#include <iosfwd>

#include "multio/DataSink.h"
#include "multio/FileSink.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/DataHandle.h"

using namespace eckit;

//----------------------------------------------------------------------------------------------------------------------

namespace multio {

FileSink::FileSink(const Configuration& config) :
    DataSink(config),
    path_(config.getString("path")),
    handle_(path_.fileHandle(false)) {
    handle_->openForWrite(0);
}

FileSink::~FileSink() {
    handle_->close();
}

void FileSink::write(eckit::DataBlobPtr blob) {
    size_t length = blob->length();

    std::lock_guard<std::mutex> lock(mutex_);

    if (size_t(handle_->write(blob->buffer(), length)) != length) {
        throw WriteError(std::string("Write error on file: ") + path_, Here());
    }
}

void FileSink::flush() {
    std::cout << "Flush is called..." << std::endl;
    handle_->flush();
}

void FileSink::print(std::ostream& os) const {
    os << "FileSink(path=" << path_ << ")";
}

static DataSinkBuilder<FileSink> FileSinkFactorySingleton("file");

}  // namespace multio
