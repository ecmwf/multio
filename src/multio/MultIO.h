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


#ifndef multio_MultIO_H
#define multio_MultIO_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/io/Length.h"

#include "eckit/multiplexer/DataSink.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class MultIO : private eckit::multiplexer::DataSink {

public:

    MultIO();

    virtual ~MultIO();

    virtual void open(const std::string& key);

    virtual void write(const void* buffer, const eckit::Length& length);

    virtual void close();

protected:

    virtual void print(std::ostream&) const;

private:

    friend std::ostream &operator<<(std::ostream &s, const MultIO &p) {
        p.print(s);
        return s;
    }

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif

