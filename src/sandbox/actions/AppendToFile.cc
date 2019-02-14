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
namespace sandbox {
namespace actions {

AppendToFile::AppendToFile(const eckit::Configuration& config) : Action(config) {
    if (not config.has("path"))
        throw eckit::UserError("AppendToFile config must define 'path'");

    config.get("path", path_);

    datahandle_.reset(eckit::PathName(path_).fileHandle());

    datahandle_->openForAppend(0);
}

AppendToFile::~AppendToFile() {
    if (datahandle_)
        datahandle_->close();
}

bool AppendToFile::execute(Message msg) {
    auto written = datahandle_->write(msg.payload(), long(msg.size()));
    written += datahandle_->write("\n", 1);
    return written == msg.size();
}

void AppendToFile::print(std::ostream& os) const
{
    os << "AppendToFile(path=" << path_ << ")";
}

static ActionBuilder<AppendToFile> AppendToFileBuilder("AppendToFile");

}  // namespace actions
}  // namespace sandbox
}  // namespace multio
