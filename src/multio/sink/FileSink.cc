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

#include "multio/sink/DataSink.h"
#include "multio/sink/FileSink.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/DataHandle.h"

using namespace eckit;

//----------------------------------------------------------------------------------------------------------------------

namespace multio {

FileSink::FileSink(const Configuration& config) :
    DataSink(config), path_(config_.getString("path")), handle_(path_.fileHandle(false)) {
    if (config_.getBool("append", false)) {
        handle_->openForAppend(0);
    }
    else {
        handle_->openForWrite(0);
    }
}

FileSink::~FileSink() {
    handle_->close();
}

void FileSink::write(metkit::data::Message msg) {
    std::lock_guard<std::mutex> lock(mutex_);
    msg.write(*handle_);
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
