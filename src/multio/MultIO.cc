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

using namespace eckit;
using namespace eckit::multiplexer;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

MultIO::MultIO(const eckit::Configuration& config) :
    MultiplexerSink(config) {
}

MultIO::~MultIO() {
}

//----------------------------------------------------------------------------------------------------------------------

int MultIO::iopenfdb(const char *name, const char *mode, int name_len, int mode_len) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iopenfdb(name,mode,name_len,mode_len);
    }
    return 0;
}

int MultIO::iinitfdb() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iinitfdb();
    }
    return 0;
}

int MultIO::isetcommfdb(int *rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetcommfdb(rank);
    }
    return 0;
}

int MultIO::isetrankfdb(int *rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetrankfdb(rank);
    }
    return 0;
}

int MultIO::iset_fdb_root(const char *name, int name_len) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iset_fdb_root(name,name_len);
    }
    return 0;
}

int MultIO::ireadfdb(void *data, int *words) {
    NOTIMP;
    return 0;
}

int MultIO::iflushfdb() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iflushfdb();
    }
    return 0;
}

int MultIO::isetfieldcountfdb(int *all_ranks, int *this_rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetfieldcountfdb(all_ranks,this_rank);
    }
    return 0;
}

int MultIO::isetvalfdb(const char *name, const char *value, int name_len, int value_len) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetvalfdb(name,value,name_len,value_len);
    }
    return 0;
}

void MultIO::print(std::ostream& os) const
{
    os << "MultIO(";
    MultiplexerSink::print(os);
    os << ")";
}

DataSinkBuilder<MultIO> DataSinkSinkBuilder("multio");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

