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

#include "multio/fdb4/FDB4Sink.h"

#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/exception/Exceptions.h"

//----------------------------------------------------------------------------------------------------------------------

/* FDB4 Fortran interface */

extern "C" {

typedef int fortint;

fortint iinitfdb4_(void);

fortint iopenfdb4_(const char *name, fortint *addr, const char *mode, int name_len, int mode_len);
fortint iclosefdb4_(fortint *addr);

fortint iflushfdb4_(fortint *addr);

fortint iset_fdb4_root_(fortint *addr, const char *name, int name_len);

int isetcommfdb4_(fortint *comm);
int isetrankfdb4_(fortint *addr, fortint *rank);
int isetfieldcountfdb4_(fortint *addr, fortint *all_ranks, fortint *this_rank);

fortint isetvalfdb4_(fortint *addr, const char *name, const char *value, int name_len, int value_len);

fortint iwritefdb4_(fortint *addr, void *data, fortint *words);
fortint ireadfdb4_(fortint *addr, void *data, fortint *words);

}

//----------------------------------------------------------------------------------------------------------------------

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

FDB4Sink::FDB4Sink(const Configuration& config) :
    DataSink(config) {
}

FDB4Sink::~FDB4Sink() {
}

void FDB4Sink::open()
{
    // do nothing -- see iopenfdb()
}

void FDB4Sink::write(const void* buffer, const eckit::Length& length)
{
    ASSERT(open_);

    int words = size_t(length) / sizeof(fortint);

    size_t len = words * sizeof(fortint);
    ASSERT( len == length );

    int err = iwritefdb4_(&fdb_, const_cast<void*>(buffer), &words);
}

void FDB4Sink::close()
{
    ASSERT(open_);
    int err = iclosefdb4_(&fdb_);
    open_ = false;
}

void FDB4Sink::print(std::ostream& os) const
{
    os << "FDB4Sink()";
}

int FDB4Sink::iopenfdb(const char *name, const char *mode, int name_len, int mode_len) {
    // fdb must not to be open
    ASSERT(!open_);
    int err = iopenfdb4_(name, &fdb_, mode, name_len, mode_len);
    // fdb_ is now set => fdb is open...
    open_ = true;
    return err;
}

int FDB4Sink::iinitfdb() {
    // fdb does not need to be open
    int err = iinitfdb4_();
    return err;
}

int FDB4Sink::isetcommfdb(int *comm) {
    // fdb does not need to be open
    int err = isetcommfdb4_(comm);
    return err;
}

int FDB4Sink::isetrankfdb(int *rank) {
    ASSERT(open_);
    int err = isetrankfdb4_(&fdb_, rank);
    return err;
}

int FDB4Sink::iset_fdb_root(const char *name, int name_len) {
    ASSERT(open_);
    int err = iset_fdb4_root_(&fdb_, name, name_len);
    return err;
}

int FDB4Sink::iflushfdb() {
    ASSERT(open_);
    int err = iflushfdb4_(&fdb_);
    return err;
}

int FDB4Sink::isetfieldcountfdb(int *all_ranks, int *this_rank) {
    ASSERT(open_);
    int err = isetfieldcountfdb4_(&fdb_, all_ranks, this_rank);
    return err;
}

int FDB4Sink::isetvalfdb(const char *name, const char *value, int name_len, int value_len) {
    ASSERT(open_);
    int err = isetvalfdb4_(&fdb_, name, value, name_len, value_len);
    return err;
}

DataSinkBuilder<FDB4Sink> FDB4SinkBuilder("fdb4");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

