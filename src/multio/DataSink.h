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


#ifndef multio_DataSink_H
#define multio_DataSink_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/config/Configuration.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/io/Length.h"
#include "eckit/io/Buffer.h"

#include "Journal.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

// n.b. Journal is already NonCopyable
class DataSink : public Journal {

public: // methods

    DataSink(const eckit::Configuration& config);

    virtual ~DataSink();

    // open() and its ilk ensure that journaling is correctly set up.
    // open_() implement the sink-specific functionality
    void open();
    virtual void open_() = 0;

    virtual void write(const void* buffer, const eckit::Length& length,
                       JournalRecord * const parent_record = NULL);
    virtual void write_(const void* buffer, const eckit::Length& length,
                        JournalRecord& journal_record) = 0;

    virtual void close() = 0;

    void journal(Journal *const journal) { journal_ = journal; }

    // Overridable routine that records a 'write' entry in the journal by adding
    // it to the journal record.
    //
    // --> If there is any datasink specific data, it should be added here.
    virtual void record_write_journal_entry(JournalRecord& journal_record,
                                            const void * buffer, const eckit::Length& length);

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

protected: // methods

    virtual void print(std::ostream&) const = 0;

    bool failOnError() { return failOnError_; }

private: // methods

    friend std::ostream &operator<<(std::ostream &s, const DataSink &p) {
        p.print(s);
        return s;
    }

private: // members

    bool failOnError_;
    bool journaled_;        /// Write to a journal file
    bool journalAlways_;    /// Write details to journal even if a write succeeds.

    Journal* journal_;

};

//----------------------------------------------------------------------------------------------------------------------

class DataSinkFactory {

    std::string name_;
    virtual DataSink* make(const eckit::Configuration& config) = 0;

protected:

    DataSinkFactory(const std::string&);
    virtual ~DataSinkFactory();

public:

    static void list(std::ostream &);
    static DataSink* build(const std::string&, const eckit::Configuration& config);

};

template< class T>
class DataSinkBuilder : public DataSinkFactory {

    virtual DataSink* make(const eckit::Configuration& config) {
        return new T(config);
    }

public:
    DataSinkBuilder(const std::string &name) : DataSinkFactory(name) {}
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multiplexer

#endif

