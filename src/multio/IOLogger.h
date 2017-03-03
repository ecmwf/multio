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


#ifndef multio_IOLogger_H
#define multio_IOLogger_H


#include "eckit/memory/NonCopyable.h"

#include <string>

namespace eckit {
    class Length;
}


namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class IOLogger : public eckit::NonCopyable {

public:

    IOLogger();
    ~IOLogger();

    void report(std::ostream& s) const;

    void logRead(const eckit::Length& size);
    void logWrite(const eckit::Length& size);

private: // methods

    void print(std::ostream& s) const;

private: // members

    // The data elements that we actually want to track

    size_t numReads_;
    size_t bytesRead_;
    size_t sumBytesReadSquared_;
    // timeRead_;

    // Can we track no. reads/writes separately to the the number of fields read/written
    size_t numWrites_;
    size_t bytesWritten_;
    size_t sumBytesWrittenSquared_;
    // timeWrite_;

    // ...


    // Todo: track stddev, min, max, mean on the fly.


private: // methods

    friend std::ostream &operator<<(std::ostream &s, const IOLogger &p) {
        p.print(s);
        return s;
    }

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif // multio_IOLogger_H

