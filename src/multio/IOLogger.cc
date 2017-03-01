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

#include "eckit/io/Length.h"
#include "eckit/log/Bytes.h"
#include "eckit/thread/AutoLock.h"

#include "multio/IOLogger.h"

#include <cmath>


using namespace eckit;

namespace multio {

/// @note We do not need to provide locking for access to the IOLogger.
///
///       Within the MultIO, all reads/writes/etc. are locked
///         --> locking occurs at a higher level than this!

//----------------------------------------------------------------------------------------------------------------------


IOLogger::IOLogger() :
    numReads_(0),
    bytesRead_(0),
    sumBytesReadSquared_(0),
    numWrites_(0),
    bytesWritten_(0),
    sumBytesWrittenSquared_(0) {}


IOLogger::~IOLogger() {}


void IOLogger::logRead(const eckit::Length &size) {

    numReads_++;
    bytesRead_ += size;
    sumBytesReadSquared_ += (size * size);
}


void IOLogger::logWrite(const eckit::Length &size) {

    numWrites_++;
    bytesWritten_ += size;
    sumBytesWrittenSquared_ += (size * size);
}


std::string IOLogger::report() const {

    std::stringstream s;

    if (numWrites_ != 0) {
        s << "Write statistics: " << std::endl;
        s << "    Total writes: " << numWrites_ << std::endl;
        s << "    Bytes write: " << Bytes(bytesWritten_) << std::endl;
        s << "    Average size: " << Bytes(double(bytesWritten_) / double(numWrites_)) << std::endl;

        double stddev_write = std::sqrt((numWrites_ * sumBytesWrittenSquared_ - bytesWritten_ * bytesWritten_) / numWrites_);
        s << "    Std. Deviation: " << Bytes(stddev_write);

        if (numReads_ != 0)
            s << std::endl;
    }

    // -----

    if (numReads_ != 0) {
        s << "Read statistics: " << std::endl;
        s << "    Total reads: " << numReads_ << std::endl;
        s << "    Bytes read: " << Bytes(bytesRead_) << std::endl;
        s << "    Average size: " << Bytes(double(bytesRead_) / double(numReads_)) << std::endl;

        double stddev_read = std::sqrt((numReads_ * sumBytesReadSquared_ - bytesRead_ * bytesRead_) / numReads_);
        s << "    Std. Deviation: " << Bytes(stddev_read);
    }

    return s.str();
}


void IOLogger::print(std::ostream &s) const {
    s << "IOLogger()";
}


//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

