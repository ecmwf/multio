/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "AppendToFile.h"

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/DataHandle.h"
#include "eckit/log/Log.h"

namespace multio {
namespace server {
namespace actions {

AppendToFile::AppendToFile(const eckit::Configuration& config) : Action(config) {
    if (not config.has("path")) {
        throw eckit::UserError("AppendToFile config must define 'path'");
    }

    config.get("path", path_);

    datahandle_.reset(eckit::PathName(path_).fileHandle());

    datahandle_->openForAppend(0);
}

AppendToFile::~AppendToFile() {
    if (datahandle_) {
        datahandle_->close();
    }
}

void AppendToFile::execute(Message msg) const {
    auto written = datahandle_->write(msg.payload().data(), long(msg.size()));
    written += datahandle_->write("\n", 1);

    ASSERT(written == static_cast<long>(msg.size() + 1));

    if (next_) {  // May want to assert not next_
        next_->execute(msg);
    }
}

void AppendToFile::print(std::ostream& os) const
{
    os << "AppendToFile(path=" << path_ << ")";
}

static ActionBuilder<AppendToFile> AppendToFileBuilder("AppendToFile");

}  // namespace actions
}  // namespace server
}  // namespace multio
