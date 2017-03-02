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

    IOStats();
    ~IOStats();

    void report(std::ostream& s) const;

    void logRead(const eckit::Length& size, eckit::Timer& timer);
    void logWrite(const eckit::Length& size, eckit::Timer& timer);

    // Log the IFS interfaces

    void logiinitfdb_(eckit::Timer& timer);
    void logiopenfdb_(eckit::Timer& timer);
    void logiclosefdb_(eckit::Timer& timer);
    void logiflushfdb_(eckit::Timer& timer);
    void logiwritefdb_(eckit::Timer& timer);
    void logireadfdb_(eckit::Timer& timer);

private: // methods

    void print(std::ostream& s) const;

private: // members

    // The data elements that we actually want to track

    size_t numReads_;
    size_t bytesRead_;
    size_t sumBytesReadSquared_;
    eckit::Timing readTiming_;
    double sumReadTimesSquared_;

    // Can we track no. reads/writes separately to the the number of fields read/written
    size_t numWrites_;
    size_t bytesWritten_;
    size_t sumBytesWrittenSquared_;
    eckit::Timing writeTiming_;
    double sumWriteTimesSquared_;

    // Log access counts for the legacy interface

    size_t numiinitfdb_;
    size_t numiopenfdb_;
    size_t numiclosefdb_;
    size_t numiflushfdb_;
    size_t numiwritefdb_;
    size_t numireadfdb_;

    double sumTimingSquaresiinitfdb_;
    double sumTimingSquaresiopenfdb_;
    double sumTimingSquaresiclosefdb_;
    double sumTimingSquaresiflushfdb_;
    double sumTimingSquaresiwritefdb_;
    double sumTimingSquaresireadfdb_;

    eckit::Timing timingiinitfdb_;
    eckit::Timing timingiopenfdb_;
    eckit::Timing timingiclosefdb_;
    eckit::Timing timingiflushfdb_;
    eckit::Timing timingiwritefdb_;
    eckit::Timing timingireadfdb_;

private: // methods

    friend std::ostream &operator<<(std::ostream &s, const IOStats &p) {
        p.print(s);
        return s;
    }

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif // multio_IOStats_H

