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

#include "multio/fdb4/FDB4Sink.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/parser/StringTools.h"

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
    DataSink(config) {
}

FDB4Sink::~FDB4Sink() {
}

void FDB4Sink::write(eckit::DataBlobPtr blob) {
    std::ostringstream msg;
    msg << "FDB4Sink::write() not implemented for FDB4";
    throw SeriousBug(msg.str());
}

void FDB4Sink::iwritefdb(int fdbaddr, eckit::DataBlobPtr blob) {

    size_t length = blob->buffer().size(); // we assume is a multiple of sizeof(fortint) (see ifsio.cc)

    int words = size_t(length) / sizeof(fortint);
    size_t len = words * sizeof(fortint);

    ASSERT( len == length );               // we check that indeed is a multiple of sizeof(fortint)

    if (iwritefdb4_(&fdbaddr, const_cast<Buffer&>(blob->buffer()), &words)) {
        throw WriteError("Write failed in FDBSink. File: ");
    }
}

void FDB4Sink::print(std::ostream& os) const
{
    os << "FDB4Sink()";
}

void FDB4Sink::iopenfdb(const std::string& name, int& fdbaddr, const std::string& mode) {

    fortint addr;
    int err = iopenfdb4_(name.c_str(), &addr, mode.c_str(), name.size(), mode.size());
    fdbaddr = addr;

    if(err) {
        throw SeriousBug("Multio FDB4 fail to open", Here());
    }
}

void FDB4Sink::iclosefdb(int fdbaddr)
{
    int err = iclosefdb4_(&fdbaddr);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to close", Here());
    }
}

void FDB4Sink::iinitfdb() {
    int err = iinitfdb4_();
    if(err) {
        throw SeriousBug("Multio FDB4 failed to initialize", Here());
    }
}

void FDB4Sink::isetcommfdb(int comm) {
    int err = isetcommfdb4_(&comm);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set MPI communicator", Here());
    }
}

void FDB4Sink::isetrankfdb(int fdbaddr, int rank) {
    int err = isetrankfdb4_(&fdbaddr, &rank);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set MPI rank", Here());
    }
}

void FDB4Sink::iset_fdb_root(int fdbaddr, const std::string& name) {
    int err = iset_fdb4_root_(&fdbaddr, name.c_str(), name.size());
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set FDB root", Here());
    }
}

void FDB4Sink::iflushfdb(int fdbaddr) {
    int err = iflushfdb4_(&fdbaddr);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to flush", Here());
    }
}

void FDB4Sink::isetfieldcountfdb(int fdbaddr, int all_ranks, int this_rank) {
    int err = isetfieldcountfdb4_(&fdbaddr, &all_ranks, &this_rank);
    if(err) {
        throw SeriousBug("Multio FDB4 failed to set field count", Here());
    }
}

void FDB4Sink::isetvalfdb(int fdbaddr, const std::string& name, const std::string& value) {

    int err = isetvalfdb4_(&fdbaddr, name.c_str(), value.c_str(), name.size(), value.size());
    if(err) {

        // ignore values that are set to off in FDB4
        std::string v = eckit::StringTools::lower(value);
        if(v == "off") return;

        throw SeriousBug("Multio FDB4 failed to set metadata values", Here());
    }
}

static DataSinkBuilder<FDB4Sink> FDB4SinkBuilder("fdb4");
