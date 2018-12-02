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


#ifndef multio_FileSink_H
#define multio_FileSink_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/filesystem/PathName.h"
#include "eckit/io/Length.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/thread/Mutex.h"

#include "multio/DataSink.h"

//----------------------------------------------------------------------------------------------------------------------

namespace eckit {
class FileHandle;
}

namespace multio {

class FileSink final : public DataSink {
public:
    FileSink(const eckit::Configuration& config);

    ~FileSink() override;

private:  // methods
    void write(eckit::DataBlobPtr blob) override;

    void print(std::ostream&) const override;

private:  // members
    eckit::PathName path_;
    std::unique_ptr<eckit::DataHandle> handle_;
    eckit::Mutex mutex_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif  // multio_FileSink_H
