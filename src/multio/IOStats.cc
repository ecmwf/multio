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
#include "eckit/log/Statistics.h"
#include "eckit/log/Timer.h"
#include "eckit/thread/AutoLock.h"

#include "multio/IOStats.h"

#include <cmath>


using namespace eckit;

namespace multio {

/// @note We do not need to provide locking for access to the IOStats.
///
///       Within the MultIO, all reads/writes/etc. are locked
///         --> locking occurs at a higher level than this!

//----------------------------------------------------------------------------------------------------------------------


IOStats::IOStats() :
    numReads_(0),
    bytesRead_(0),
    sumBytesReadSquared_(0),
    numWrites_(0),
    bytesWritten_(0),
    sumBytesWrittenSquared_(0),
    numiinitfdb_(0),
    numiopenfdb_(0),
    numiclosefdb_(0),
    numiflushfdb_(0),
    numiwritefdb_(0),
    numireadfdb_(0) {}


IOStats::~IOStats() {}


void IOStats::logRead(const Length &size, Timer& timer) {

    numReads_++;
    bytesRead_ += size;
    sumBytesReadSquared_ += (size * size);
    readTiming_ += timer;
}


void IOStats::logWrite(const Length &size, Timer& timer) {

    numWrites_++;
    bytesWritten_ += size;
    sumBytesWrittenSquared_ += (size * size);
    writeTiming_ += timer;
}


void IOStats::logiinitfdb_(eckit::Timer& timer) {

    numiinitfdb_++;
    timingiinitfdb_ += timer;
}


void IOStats::logiopenfdb_(eckit::Timer& timer) {

    numiopenfdb_++;
    timingiopenfdb_ += timer;
}


void IOStats::logiclosefdb_(eckit::Timer& timer) {

    numiclosefdb_++;
    timingiclosefdb_ += timer;
}


void IOStats::logiflushfdb_(eckit::Timer& timer) {

    numiflushfdb_++;
    timingiflushfdb_ += timer;
}


void IOStats::logiwritefdb_(eckit::Timer& timer) {

    numiwritefdb_++;
    timingiwritefdb_ += timer;
}


void IOStats::logireadfdb_(eckit::Timer& timer) {

    numireadfdb_++;
    timingireadfdb_ += timer;
}


void IOStats::report(std::ostream& s) const {

    if (numWrites_ != 0) {
        s << "Write statistics: " << std::endl;
        Statistics::reportCount(s, "Writes", numWrites_);
        Statistics::reportBytes(s, "Written", bytesWritten_);
        Statistics::reportBytes(s, "Av. size", size_t(double(bytesWritten_) / double(numWrites_)));

        double stddev_write = std::sqrt((numWrites_ * sumBytesWrittenSquared_ - bytesWritten_ * bytesWritten_)) / numWrites_;
        Statistics::reportBytes(s, "Std. dev.", size_t(stddev_write));
        Statistics::reportTime(s, "Time: ", writeTiming_);

        Timing timingCopy = writeTiming_;
        timingCopy /= numWrites_;
        Statistics::reportTime(s, "Av. time", timingCopy);

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
        Statistics::reportTime(s, "Time", readTiming_);

        Timing timingCopy = readTiming_;
        timingCopy /= numReads_;
        Statistics::reportTime(s, "Av. time", timingCopy);
    }

    // Output for IFS statistics

    if (numiinitfdb_ != 0) {
        Statistics::reportCount(s, "No. iinitfdb", numiinitfdb_);
        Statistics::reportTime(s, "Time iinitfdb", timingiinitfdb_);
    }
    if (numiinitfdb_ != 0) {
        Statistics::reportCount(s, "No. iinitfdb", numiinitfdb_);
        Statistics::reportTime(s, "Time iinitfdb", timingiinitfdb_);
    }
    if (numiopenfdb_ != 0) {
        Statistics::reportCount(s, "No. iopenfdb", numiopenfdb_);
        Statistics::reportTime(s, "Time iopenfdb", timingiopenfdb_);
    }
    if (numiclosefdb_ != 0) {
        Statistics::reportCount(s, "No. iclosefdb", numiclosefdb_);
        Statistics::reportTime(s, "Time iclosefdb", timingiclosefdb_);
    }
    if (numiflushfdb_ != 0) {
        Statistics::reportCount(s, "No. iflushfdb", numiflushfdb_);
        Statistics::reportTime(s, "Time iflushfdb", timingiflushfdb_);
    }
    if (numiwritefdb_ != 0) {
        Statistics::reportCount(s, "No. iwritefdb", numiwritefdb_);
        Statistics::reportTime(s, "Time iwritefdb", timingiwritefdb_);
    }
    if (numireadfdb_ != 0) {
        Statistics::reportCount(s, "No. ireadfdb", numireadfdb_);
        Statistics::reportTime(s, "Time ireadfdb", timingireadfdb_);
    }
}


void IOStats::print(std::ostream &s) const {
    s << "IOStats()";
}


//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

