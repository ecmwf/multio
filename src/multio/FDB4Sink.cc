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

FDB4Sink::FDB4Sink(const Configuration& config) :
    DataSink(config) {
}

FDB4Sink::~FDB4Sink() {
}

void FDB4Sink::open_()
{

}

void FDB4Sink::write_(const void* buffer, const eckit::Length& length, JournalRecord& journal_record)
{

}

void FDB4Sink::close()
{

}

void FDB4Sink::print(std::ostream& os) const
{
    os << "FDB4Sink()";
}

int FDB4Sink::iopenfdb(const char *name, const char *mode, int name_len, int mode_len) {

}

int FDB4Sink::iinitfdb() {

}

int FDB4Sink::isetcommfdb(int *rank) {

}

int FDB4Sink::isetrankfdb(int *rank) {

}

int FDB4Sink::iset_fdb_root(const char *name, int name_len) {

}

int FDB4Sink::iflushfdb() {

}

int FDB4Sink::isetfieldcountfdb(int *all_ranks, int *this_rank) {

}

int FDB4Sink::isetvalfdb(const char *name, const char *value, int name_len, int value_len) {

}

DataSinkBuilder<FDB4Sink> FDB4SinkBuilder("fdb4");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

