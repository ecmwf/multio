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
/// @date Dec 2015


#ifndef multio_IOStats_H
#define multio_IOStats_H


#include "eckit/log/Statistics.h"
#include "eckit/memory/NonCopyable.h"

#include <string>

namespace eckit {
    class Length;
    class Timer;
}


namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class IOStats : public eckit::NonCopyable {

public:

    IOStats(const std::string& prefix=std::string());
    ~IOStats();

    void report(std::ostream& s) const;

    void logRead(const eckit::Length& size, eckit::Timer& timer);
    void logWrite(const eckit::Length& size, eckit::Timer& timer);
    void logFlush(eckit::Timer& timer);

    // Log the legacy interfaces

    void logiinitfdb_(eckit::Timer& timer);
    void logiopenfdb_(eckit::Timer& timer);
    void logiclosefdb_(eckit::Timer& timer);
    void logiflushfdb_(eckit::Timer& timer);
    void logiwritefdb_(const eckit::Length& size, eckit::Timer& timer);
    void logireadfdb_(eckit::Timer& timer);
    void logisetvalfdb_(eckit::Timer& timer);

private: // methods

    void print(std::ostream& s) const;

    void reportCount(std::ostream& s, const std::string& label, size_t num) const;
    void reportBytes(std::ostream& s, const std::string& label, size_t num, size_t sum, size_t sumSquares) const;
    void reportTimes(std::ostream& s,
                     const std::string& label,
                     size_t num,
                     const eckit::Timing& sum,
                     double sumSquares) const;

private: // members

    std::string prefix_;

    // The data elements that we actually want to track

    size_t numReads_;
    size_t bytesRead_;
    size_t sumBytesReadSquared_;
    eckit::Timing readTiming_;
    double sumReadTimesSquared_;

    size_t numWrites_;
    size_t bytesWritten_;
    size_t sumBytesWrittenSquared_;
    eckit::Timing writeTiming_;
    double sumWriteTimesSquared_;

    size_t numFlush_;
    eckit::Timing flushTiming_;
    double sumFlushTimesSquared_;

    // Log access counts for the legacy interface

    size_t numiinitfdb_;
    size_t numiopenfdb_;
    size_t numiclosefdb_;
    size_t numiflushfdb_;
    size_t numiwritefdb_;
    size_t numireadfdb_;
    size_t numisetvalfdb_;

    double sumTimingSquaresiinitfdb_;
    double sumTimingSquaresiopenfdb_;
    double sumTimingSquaresiclosefdb_;
    double sumTimingSquaresiflushfdb_;
    double sumTimingSquaresiwritefdb_;
    double sumTimingSquaresireadfdb_;
    double sumTimingSquaresisetvalfdb_;

    eckit::Timing timingiinitfdb_;
    eckit::Timing timingiopenfdb_;
    eckit::Timing timingiclosefdb_;
    eckit::Timing timingiflushfdb_;
    eckit::Timing timingiwritefdb_;
    eckit::Timing timingireadfdb_;
    eckit::Timing timingisetvalfdb_;

    size_t iwritefdbBytesWritten_;
    size_t iwritefdbSumBytesWrittenSquared_;

private: // methods

    friend std::ostream &operator<<(std::ostream &s, const IOStats &p) {
        p.print(s);
        return s;
    }

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif // multio_IOStats_H

