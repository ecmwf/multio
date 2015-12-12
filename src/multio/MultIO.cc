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

#include "multio/MultIO.h"

#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/exception/Exceptions.h"

#include "multio/FDB4.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

MultIO::MultIO(const eckit::Configuration& config) :
    MultiplexerSink(config),
    fdb4_(new FDB4(config)) {
}

MultIO::~MultIO() {
}

void MultIO::open(const std::string& key) {

    fdb4().open(key);

    MultiplexerSink::open(key);
}

void MultIO::write(const void* buffer, const eckit::Length& length) {

    fdb4().write(buffer,length);

    MultiplexerSink::write(buffer,length);
}

void MultIO::close() {

    fdb4().close();

    MultiplexerSink::close();

}

FDB4& MultIO::fdb4() const
{
    ASSERT(fdb4_);

    return *fdb4_;
}

void MultIO::print(std::ostream&) const
{
    NOTIMP;
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

