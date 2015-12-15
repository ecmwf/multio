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

#include "multio/FDB4Sink.h"

#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/exception/Exceptions.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

FDB4Sink::FDB4Sink(const Configuration& config) : DataSink() {}

FDB4Sink::~FDB4Sink() {
}

void FDB4Sink::open(const std::string& key)
{

}

void FDB4Sink::write(const void* buffer, const eckit::Length& length)
{

}

void FDB4Sink::close()
{

}

void FDB4Sink::print(std::ostream& os) const
{
    os << "FDB4Sink()";
}

int FDB4Sink::iclosefdb(int *addr) {

}

int FDB4Sink::iopenfdb(const char *name, int *addr, const char *mode, int name_len, int mode_len) {

}

int FDB4Sink::iinitfdb(void) {

}

int FDB4Sink::isetcommfdb(int *rank) {

}

int FDB4Sink::isetrankfdb(int *addr, int *rank) {

}

int FDB4Sink::iset_fdb_root(int *addr, const char *name, int name_len) {

}

int FDB4Sink::ireadfdb(int *addr, void *data, int *words) {

}

int FDB4Sink::iflushfdb(int *addr) {

}

int FDB4Sink::isetfieldcountfdb(int *addr, int *all_ranks, int *this_rank) {

}

int FDB4Sink::isetvalfdb(int *addr, const char *name, const char *value, int name_len, int value_len) {

}

DataSinkBuilder<FDB4Sink> FDB4SinkBuilder("fdb4");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

