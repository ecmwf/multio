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


#ifndef multio_FDB4_H
#define multio_FDB4_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/io/Length.h"

#include "eckit/multiplexer/DataSink.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class FDB4 : private eckit::multiplexer::DataSink {

public:

    FDB4(const eckit::Configuration& config);

    virtual ~FDB4();

    virtual void open(const std::string& key);

    virtual void write(const void* buffer, const eckit::Length& length);

    virtual void close();

    ///
    /// LEGACY INTERFACE TO REMOVE AFTER IFS CHANGED TO LEAN WRITE() INTERFACE
    ///

    int iclosefdb(int *addr);
    int iopenfdb(const char *name, int *addr, const char *mode, int name_len, int mode_len);
    int iinitfdb(void);

    int isetcommfdb(int *rank);
    int isetrankfdb(int *addr, int *rank);
    int iset_fdb_root(int *addr, const char *name, int name_len);

    int ireadfdb(int *addr, void *data, int *words);
    int iwritefdb(int *addr, void *data, int *words);
    int iflushfdb(int *addr);

    int isetfieldcountfdb(int *addr, int *all_ranks, int *this_rank);
    int isetvalfdb(int *addr, const char *name, const char *value, int name_len, int value_len);

protected:

    virtual void print(std::ostream&) const;

private:

    friend std::ostream &operator<<(std::ostream &s, const FDB4 &p) {
        p.print(s);
        return s;
    }

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif

