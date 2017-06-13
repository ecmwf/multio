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
#include "eckit/log/BigNum.h"
#include "eckit/log/Bytes.h"
#include "eckit/log/Timer.h"
#include "eckit/thread/AutoLock.h"

#include "multio/IOStats.h"
#include "multio/LibMultio.h"

#include <cmath>

static const int FORMAT_WIDTH = 42;


using namespace eckit;

namespace multio {

/// @note We do not need to provide locking for access to the IOStats.
///
///       Within the MultIO, all reads/writes/etc. are locked
///         --> locking occurs at a higher level than this!

//----------------------------------------------------------------------------------------------------------------------


IOStats::IOStats(const std::string& prefix) :
    prefix_(prefix),
    numReads_(0),
    bytesRead_(0),
    sumBytesReadSquared_(0),
    sumReadTimesSquared_(0),
    numWrites_(0),
    bytesWritten_(0),
    sumBytesWrittenSquared_(0),
    sumWriteTimesSquared_(0),
    numFlush_(0),
    sumFlushTimesSquared_(0),
    numiinitfdb_(0),
    numiopenfdb_(0),
    numiclosefdb_(0),
    numiflushfdb_(0),
    numiwritefdb_(0),
    numireadfdb_(0),
    numisetvalfdb_(0),
    sumTimingSquaresiinitfdb_(0),
    sumTimingSquaresiopenfdb_(0),
    sumTimingSquaresiclosefdb_(0),
    sumTimingSquaresiflushfdb_(0),
    sumTimingSquaresiwritefdb_(0),
    sumTimingSquaresireadfdb_(0),
    sumTimingSquaresisetvalfdb_(0),
    iwritefdbBytesWritten_(0),
    iwritefdbSumBytesWrittenSquared_(0) {

    if (!prefix_.empty())
        prefix_ += std::string(" ");
}


IOStats::~IOStats() {}


void IOStats::logRead(const Length &size, Timer& timer) {

    numReads_++;
    bytesRead_ += size;
    sumBytesReadSquared_ += (size * size);
    readTiming_ += timer;

    double elapsed = timer.elapsed();
    sumReadTimesSquared_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "Read count: " << numReads_
                            << ", size: " << Bytes(size)
                            << ", total: " << Bytes(bytesRead_)
                            << ", time: " << elapsed << "s"
                            << ", total: " << readTiming_.elapsed_ << "s" << std::endl;
}


void IOStats::logWrite(const Length &size, Timer& timer) {

    numWrites_++;
    bytesWritten_ += size;
    sumBytesWrittenSquared_ += (size * size);
    writeTiming_ += timer;

    double elapsed = timer.elapsed();
    sumWriteTimesSquared_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "Write count: " << numWrites_
                            << ", size: " << Bytes(size)
                            << ", total: " << Bytes(bytesWritten_)
                            << ", time: " << elapsed << "s"
                            << ", total: " << writeTiming_.elapsed_ << "s" << std::endl;
}


void IOStats::logFlush(Timer& timer) {

    numFlush_++;
    flushTiming_ += timer;

    double elapsed = timer.elapsed();
    sumFlushTimesSquared_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "Flush count: " << numFlush_
                            << ", time: " << elapsed << "s"
                            << ", total: " << flushTiming_.elapsed_ << "s" << std::endl;
}


void IOStats::logiinitfdb_(eckit::Timer& timer) {

    numiinitfdb_++;
    timingiinitfdb_ += timer;

    double elapsed = timer.elapsed();
    sumTimingSquaresiinitfdb_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "iinitfdb count: " << numiinitfdb_
                            << ", time: " << elapsed << "s"
                            << ", total: " << timingiinitfdb_.elapsed_ << "s" << std::endl;
}


void IOStats::logiopenfdb_(eckit::Timer& timer) {

    numiopenfdb_++;
    timingiopenfdb_ += timer;

    double elapsed = timer.elapsed();
    sumTimingSquaresiopenfdb_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "iopenfdb count: " << numiopenfdb_
                            << ", time: " << elapsed << "s"
                            << ", total: " << timingiopenfdb_.elapsed_ << "s" << std::endl;
}


void IOStats::logiclosefdb_(eckit::Timer& timer) {

    numiclosefdb_++;
    timingiclosefdb_ += timer;

    double elapsed = timer.elapsed();
    sumTimingSquaresiclosefdb_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "iclosefdb count: " << numiclosefdb_
                            << ", time: " << elapsed << "s"
                            << ", total: " << timingiclosefdb_.elapsed_ << "s" << std::endl;
}


void IOStats::logiflushfdb_(eckit::Timer& timer) {

    numiflushfdb_++;
    timingiflushfdb_ += timer;

    double elapsed = timer.elapsed();
    sumTimingSquaresiflushfdb_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "iflushfdb count: " << numiflushfdb_
                            << ", time: " << elapsed << "s"
                            << ", total: " << timingiflushfdb_.elapsed_ << "s" << std::endl;
}


void IOStats::logiwritefdb_(const eckit::Length& size, eckit::Timer& timer) {

    numiwritefdb_++;
    iwritefdbBytesWritten_ += size;
    iwritefdbSumBytesWrittenSquared_ += (size * size);
    timingiwritefdb_ += timer;

    double elapsed = timer.elapsed();
    sumTimingSquaresiwritefdb_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "iwritefdb count: " << numiwritefdb_
                            << ", time: " << elapsed << "s"
                            << ", total: " << timingiwritefdb_.elapsed_ << "s" << std::endl;
}


void IOStats::logireadfdb_(eckit::Timer& timer) {

    numireadfdb_++;
    timingireadfdb_ += timer;

    double elapsed = timer.elapsed();
    sumTimingSquaresireadfdb_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "ireadfdb count: " << numireadfdb_
                            << ", time: " << elapsed << "s"
                            << ", total: " << timingireadfdb_.elapsed_ << "s" << std::endl;
}

void IOStats::logisetvalfdb_(Timer &timer) {

    numisetvalfdb_++;
    timingisetvalfdb_ += timer;

    double elapsed = timer.elapsed();
    sumTimingSquaresisetvalfdb_ += elapsed * elapsed;

    Log::debug<LibMultio>() << "isetfdb count: " << numisetvalfdb_
                            << ", time: " << elapsed << "s"
                            << ", total: " << timingisetvalfdb_.elapsed_ << "s" << std::endl;

}


void IOStats::report(std::ostream& s) const {

    // Write statistics

    reportCount(s, "num writes", numWrites_);
    reportBytes(s, "bytes written", numWrites_, bytesWritten_, sumBytesWrittenSquared_);
    reportTimes(s, "write time", numWrites_, writeTiming_, sumWriteTimesSquared_);
    reportRate(s, "write rate", bytesWritten_, writeTiming_);

    // Read statistics

    reportCount(s, "num reads", numReads_);
    reportBytes(s, "bytes read", numReads_, bytesRead_, sumBytesReadSquared_);
    reportTimes(s, "read time", numReads_, readTiming_, sumReadTimesSquared_);
    reportRate(s, "read rate", bytesRead_, readTiming_);

    // Flush statistics

    reportCount(s, "num flush", numFlush_);
    reportTimes(s, "flush time", numFlush_, flushTiming_, sumFlushTimesSquared_);

    // Output for legacy interface statistics

    reportCount(s, "num iinitfdb", numiinitfdb_);
    reportTimes(s, "time iinitfdb", numiinitfdb_, timingiinitfdb_, sumTimingSquaresiinitfdb_);

    reportCount(s, "num iopenfdb", numiopenfdb_);
    reportTimes(s, "time iopenfdb", numiopenfdb_, timingiopenfdb_, sumTimingSquaresiopenfdb_);

    reportCount(s, "num iclosefdb", numiclosefdb_);
    reportTimes(s, "time iclosefdb", numiclosefdb_, timingiclosefdb_, sumTimingSquaresiclosefdb_);

    reportCount(s, "num iflushfdb", numiflushfdb_);
    reportTimes(s, "time iflushfdb", numiflushfdb_, timingiflushfdb_, sumTimingSquaresiflushfdb_);

    reportCount(s, "num isetvalfdb", numisetvalfdb_);
    reportTimes(s, "time isetvalfdb", numisetvalfdb_, timingisetvalfdb_, sumTimingSquaresisetvalfdb_);

    reportCount(s, "num iwritefdb", numiwritefdb_);
    reportBytes(s, "bytes iwritefdb", numiwritefdb_, iwritefdbBytesWritten_, iwritefdbSumBytesWrittenSquared_);
    reportTimes(s, "time iwritefdb", numiwritefdb_, timingiwritefdb_, sumTimingSquaresiwritefdb_);
    reportRate(s, "rate iwritefdb", iwritefdbBytesWritten_, timingiwritefdb_);

//    reportCount(s, "num ireadfdb", numireadfdb_);
//    reportBytes(s, "bytes ireadfdb", numireadfdb_, ireadfdbBytesWritten_, ireadfdbSumBytesWrittenSquared_);
//    reportTimes(s, "time ireadfdb", numireadfdb_, timingireadfdb_, sumTimingSquaresireadfdb_);

}


void IOStats::print(std::ostream &s) const {
    s << "IOStats()";
}

void IOStats::reportCount(std::ostream& s, const std::string &label, size_t num) const {

    s << prefix_
      << label
      << std::setw(FORMAT_WIDTH - label.length())
      << " : "
      << BigNum(num)
      << std::endl;
}

void IOStats::reportBytes(std::ostream& s, const std::string &label, size_t num, size_t sum, size_t sumSquares) const {

    std::string lbl = label + " (tot, avg, std dev)";

    double average = 0;
    double stdDeviation = 0;
    if (num != 0) {
        average = sum / num;
        stdDeviation = std::sqrt(std::max((num * sumSquares) - (sum * sum), size_t(0))) / num;
    }

    s << prefix_
      << lbl
      << std::setw(FORMAT_WIDTH - lbl.length())
      << " : "
      << BigNum(sum) << " (" << Bytes(sum) << ")"
      << ", " << BigNum(size_t(average)) << " (" << Bytes(average) << ")"
      << ", " << BigNum(size_t(stdDeviation)) << " (" << Bytes(stdDeviation) << ")"
      << std::endl;
}

void IOStats::reportTimes(std::ostream& s,
                          const std::string &label,
                          size_t num,
                          const Timing& sum,
                          double sumSquares) const {

    double elapsed = sum.elapsed_;
    std::string lbl = label + " (tot, avg, std dev)";

    double average = 0;
    double stdDeviation = 0;
    if (num != 0) {
        average = elapsed / num;
        stdDeviation = std::sqrt(std::max((num * sumSquares) - (elapsed * elapsed), 0.0)) / num;
    }

    s << prefix_
      << lbl
      << std::setw(FORMAT_WIDTH - lbl.length())
      << " : "
      << elapsed << "s"
      << ", " << average << "s"
      << ", " << stdDeviation << "s"
      << std::endl;
}

void IOStats::reportRate(std::ostream &s, const std::string& label, size_t bytes, const Timing &time) const {

    double elapsed = time.elapsed_;
    double rate = 0;

    if (bytes != 0 && elapsed > 0) {
        rate = bytes / elapsed;
    }

    s << prefix_
      << label
      << std::setw(FORMAT_WIDTH - label.length())
      << " : "
      << Bytes(rate) << " per second"
      << std::endl;
}


//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

