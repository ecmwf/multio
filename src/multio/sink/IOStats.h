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
/// @date Dec 2015

#pragma once

#include <string>

#include "eckit/log/Statistics.h"
#include "eckit/memory/NonCopyable.h"

namespace eckit {
class Length;
class Timer;
}  // namespace eckit


namespace multio::sink {

//----------------------------------------------------------------------------------------------------------------------

class IOStats : public eckit::NonCopyable {

public:
    IOStats(const std::string& prefix = std::string());
    ~IOStats();

    void report(std::ostream& s) const;

    void logRead(const eckit::Length& size, eckit::Timer& timer);
    void logWrite(const eckit::Length& size, eckit::Timer& timer);
    void logFlush(eckit::Timer& timer);

private:  // methods
    void print(std::ostream& s) const;

    void reportCount(std::ostream& s, const std::string& label, size_t num) const;
    void reportBytes(std::ostream& s, const std::string& label, size_t num, size_t sum, size_t sumSquares) const;
    void reportTimes(std::ostream& s, const std::string& label, size_t num, const eckit::Timing& sum,
                     double sumSquares) const;
    void reportRate(std::ostream& s, const std::string& label, size_t bytes, const eckit::Timing& time) const;

private:  // members
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

private:  // methods
    friend std::ostream& operator<<(std::ostream& s, const IOStats& p) {
        p.print(s);
        return s;
    }
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::sink
