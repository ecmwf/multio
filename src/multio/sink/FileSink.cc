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
#include "multio/util/logfile_name.h"

#include "eckit/io/DataHandle.h"

using namespace eckit;

//----------------------------------------------------------------------------------------------------------------------

namespace multio::sink {

namespace {
std::string create_path(const config::ComponentConfiguration& compConf) {
    const auto& cfg = compConf.parsedConfig();
    auto path = cfg.getString("path");
    auto expanded_path = compConf.multioConfig().replaceCurly(path);
    eckit::Log::info() << "path = " << expanded_path << std::endl;
    if (cfg.getBool("per-server", false)) {
        eckit::PathName tmp = expanded_path;
        auto dirName = tmp.baseName().asString() == expanded_path ? "" : tmp.dirName().asString() + "/";
        return dirName + util::filename_prefix() + "-" + tmp.baseName().asString();
    }
    return expanded_path;
}
}  // namespace


FileSink::FileSink(const config::ComponentConfiguration& compConf) :
    DataSink(compConf), path_{create_path(compConf)}, handle_(path_.fileHandle(false)) {
    if (compConf.parsedConfig().getBool("append", false)) {
        handle_->openForAppend(0);
    }
    else {
        handle_->openForWrite(0);
    }
}

FileSink::~FileSink() {
    handle_->close();
}

void FileSink::write(eckit::message::Message msg) {
    std::lock_guard<std::mutex> lock(mutex_);
    msg.write(*handle_);
}

void FileSink::flush() {
    eckit::Log::info() << "Flushing ";
    print(eckit::Log::info());
    eckit::Log::info() << std::endl;
    handle_->flush();
}

void FileSink::print(std::ostream& os) const {
    os << "FileSink(path=" << path_ << ")";
}

static DataSinkBuilder<FileSink> FileSinkFactorySingleton("file");

}  // namespace multio
