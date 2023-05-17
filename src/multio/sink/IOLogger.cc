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

#include "eckit/io/Length.h"
#include "eckit/log/Bytes.h"
#include "eckit/log/Statistics.h"
#include "eckit/thread/AutoLock.h"

#include "multio/IOLogger.h"

#include <cmath>


using namespace eckit;

namespace multio::sink {

/// @note We do not need to provide locking for access to the IOLogger.
///
///       Within the MultIO, all reads/writes/etc. are locked
///         --> locking occurs at a higher level than this!

//----------------------------------------------------------------------------------------------------------------------


IOLogger::IOLogger() :
    numReads_(0), bytesRead_(0), sumBytesReadSquared_(0), numWrites_(0), bytesWritten_(0), sumBytesWrittenSquared_(0) {}


IOLogger::~IOLogger() {}


void IOLogger::logRead(const eckit::Length& size) {

    numReads_++;
    bytesRead_ += size;
    sumBytesReadSquared_ += (size * size);
}


void IOLogger::logWrite(const eckit::Length& size) {

    numWrites_++;
    bytesWritten_ += size;
    sumBytesWrittenSquared_ += (size * size);
}


void IOLogger::report(std::ostream& s) const {

    if (numWrites_ != 0) {
        s << "Write statistics: " << std::endl;
        Statistics::reportCount(s, "Writes", numWrites_);
        Statistics::reportBytes(s, "Written", bytesWritten_);
        Statistics::reportBytes(s, "Av. size", size_t(double(bytesWritten_) / double(numWrites_)));

        double stddev_write
            = std::sqrt((numWrites_ * sumBytesWrittenSquared_ - bytesWritten_ * bytesWritten_)) / numWrites_;
        Statistics::reportBytes(s, "Std. dev.", size_t(stddev_write));

        if (numReads_ != 0)
            s << std::endl;
    }

    // -----

    if (numReads_ != 0) {
        s << "Read statistics: " << std::endl;
        Statistics::reportCount(s, "Reads", numReads_);
        Statistics::reportBytes(s, "Read", bytesRead_);
        Statistics::reportBytes(s, "Av. size", size_t(double(bytesRead_) / double(numReads_)));

        double stddev_read = std::sqrt((numReads_ * sumBytesReadSquared_ - bytesRead_ * bytesRead_)) / numReads_;
        Statistics::reportBytes(s, "Std. dev.", size_t(stddev_read));
    }
}


void IOLogger::print(std::ostream& s) const {
    s << "IOLogger()";
}


//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::sink
