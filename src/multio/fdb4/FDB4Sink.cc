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

#include "multio/fdb4/FDB4Sink.h"

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

fortint iwritefdb4_(fortint *addr, const void *data, fortint *words);
fortint ireadfdb4_(fortint *addr, void *data, fortint *words);

}

//----------------------------------------------------------------------------------------------------------------------

using namespace eckit;
using namespace multio;

//----------------------------------------------------------------------------------------------------------------------

FDB4Sink::FDB4Sink(const Configuration& config) :
    DataSink(config),
    fdb_(0),
    open_(false) {
}

FDB4Sink::~FDB4Sink() {
}

void FDB4Sink::write(DataBlobPtr blob, JournalRecordPtr record) {

    ASSERT(open_);

    if (record && journalAlways_) {
        record->addWriteEntry(blob, id_);
    }

    size_t length = blob->length();

    int words = size_t(length) / sizeof(fortint);

    size_t len = words * sizeof(fortint);
    ASSERT( len == length );

    int err = iwritefdb4_(&fdb_, const_cast<Buffer&>(blob->buffer()), &words);
    if (err) {
        if (record && !journalAlways_)
            record->addWriteEntry(blob, id_);
        else
            throw WriteError("Write failed in FDBSink. File: ");
    }
}

void FDB4Sink::print(std::ostream& os) const
{
    os << "FDB4Sink()";
}

void FDB4Sink::iopenfdb(const std::string& name, const std::string& mode) {
    // fdb must not to be open
    ASSERT(!open_);
    int err = iopenfdb4_(name.c_str(), &fdb_, mode.c_str(), name.size(), mode.size());
    // fdb_ is now set => fdb is open...
    open_ = true;

    if(err) {
        throw SeriousBug("Multio FDB4 fail to open", Here());
    }
}

void FDB4Sink::iclosefdb()
{
    ASSERT(open_);
    int err = iclosefdb4_(&fdb_);
    open_ = false;
    if(err) {
        throw SeriousBug("Multio FDB4 failed to close", Here());
    }
}

void FDB4Sink::iinitfdb() {
    // fdb does not need to be open
    int err = iinitfdb4_();
    if(err) {
        throw SeriousBug("Multio FDB4 failed to initialize", Here());
    }
}

void FDB4Sink::isetcommfdb(int comm) {
    // fdb does not need to be open
    int err = isetcommfdb4_(&comm);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set MPI communicator", Here());
    }
}

void FDB4Sink::isetrankfdb(int rank) {
    ASSERT(open_);
    int err = isetrankfdb4_(&fdb_, &rank);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set MPI rank", Here());
    }
}

void FDB4Sink::iset_fdb_root(const std::string& name) {
    ASSERT(open_);
    int err = iset_fdb4_root_(&fdb_, name.c_str(), name.size());
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set FDB root", Here());
    }
}

void FDB4Sink::iflushfdb() {
    ASSERT(open_);
    int err = iflushfdb4_(&fdb_);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to flush", Here());
    }
}

void FDB4Sink::isetfieldcountfdb(int all_ranks, int this_rank) {
    ASSERT(open_);
    int err = isetfieldcountfdb4_(&fdb_, &all_ranks, &this_rank);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set field count", Here());
    }
}

void FDB4Sink::isetvalfdb(const std::string& name, const std::string& value) {
    ASSERT(open_);
    int err = isetvalfdb4_(&fdb_, name.c_str(), value.c_str(), name.size(), value.size());
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set metadata values", Here());
    }
}

static DataSinkBuilder<FDB4Sink> FDB4SinkBuilder("fdb4");

