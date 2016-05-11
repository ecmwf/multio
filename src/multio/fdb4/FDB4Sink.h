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


#ifndef multio_fdb4_FDB4Sink_H
#define multio_fdb4_FDB4Sink_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/io/Length.h"

#include "multio/DataSink.h"

//----------------------------------------------------------------------------------------------------------------------

class FDB4Sink : public multio::DataSink {

public:

    FDB4Sink(const eckit::Configuration& config);

    virtual ~FDB4Sink();

    virtual void write(eckit::DataBlobPtr blob, multio::JournalRecordPtr record);

    ///
    /// LEGACY INTERFACE TO REMOVE AFTER IFS CHANGED TO SIMPLE WRITE() INTERFACE
    ///

    virtual void iopenfdb(const std::string& name, const std::string& mode);
    virtual void iinitfdb();
    virtual void iclosefdb();

    virtual void isetcommfdb(int rank);
    virtual void isetrankfdb(int rank);
    virtual void iset_fdb_root(const std::string& name);

    virtual void iflushfdb();

    virtual void isetfieldcountfdb(int all_ranks, int this_rank);
    virtual void isetvalfdb(const std::string& name, const std::string& value);

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

    int fdb_;   ///< fdb instance
    bool open_;

};

//----------------------------------------------------------------------------------------------------------------------

#endif

