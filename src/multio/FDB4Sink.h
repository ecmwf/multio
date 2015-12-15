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


#ifndef multio_FDB4Sink_H
#define multio_FDB4Sink_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/io/Length.h"

#include "multio/DataSink.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class FDB4Sink : public DataSink {

public:

    FDB4Sink(const eckit::Configuration& config);

    virtual ~FDB4Sink();

    virtual void open();

    virtual void write(const void* buffer, const eckit::Length& length);

    virtual void close();

    ///
    /// LEGACY INTERFACE TO REMOVE AFTER IFS CHANGED TO SIMPLE WRITE() INTERFACE
    ///

    virtual int iopenfdb(const char *name, const char *mode, int name_len, int mode_len);
    virtual int iinitfdb();
    // virtual int iclosefdb();

    virtual int isetcommfdb(int *rank);
    virtual int isetrankfdb(int *rank);
    virtual int iset_fdb_root(const char *name, int name_len);

    virtual int iflushfdb();

    virtual int isetfieldcountfdb(int *all_ranks, int *this_rank);
    virtual int isetvalfdb(const char *name, const char *value, int name_len, int value_len);

    // virtual int ireadfdb(void *data, int *words);
    // virtual iwritefdb(void *data, int *words);

protected:

    virtual void print(std::ostream&) const;

private:

    friend std::ostream &operator<<(std::ostream &s, const FDB4Sink &p) {
        p.print(s);
        return s;
    }

private:

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif

