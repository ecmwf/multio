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
/// @author Simon Smart
/// @date Dec 2015

#ifndef multio_DataSink_H
#define multio_DataSink_H

#include <iosfwd>
#include <mutex>
#include <string>
#include <vector>
#include <map>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/io/DataBlob.h"
#include "eckit/memory/NonCopyable.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class DataSink {
public:  // methods
    DataSink(const eckit::Configuration& config);

    virtual ~DataSink();

    virtual bool ready() const;

    virtual void write(eckit::DataBlobPtr blob) = 0;

    /// No further writes to this sink
    virtual void flush();

    /// Set the datasink ID that is used by other classes to identify this one.
    /// In particular, it labels which sink within a MultIO this one is.
    void setId(int id);
    int id() const;

    /// Return the value that is serialised to produce the json() in the journal.
    /// Not necessarily equal to the supplied config as other sources of
    /// configuration may be used. By default this just returns the supplied
    /// Configuration.
    virtual eckit::Value configValue() const;

    ///
    /// LEGACY INTERFACE TO REMOVE AFTER IFS CHANGED TO SIMPLE WRITE() INTERFACE
    ///

    virtual void iopenfdb(const std::string& name, int& fdbaddr, const std::string& mode);
    virtual void iinitfdb();
    virtual void iclosefdb(int fdbaddr);

    virtual void isetcommfdb(int rank);
    virtual void isetrankfdb(int fdbaddr, int rank);
    virtual void iset_fdb_root(int fdbaddr, const std::string& name);

    virtual void iflushfdb(int fdbaddr);

    virtual void isetfieldcountfdb(int fdbaddr, int all_ranks, int this_rank);
    virtual void isetvalfdb(int fdbaddr, const std::string& name, const std::string& value);

    // virtual int ireadfdb(void *data, int *words);
    virtual void iwritefdb(int fdbaddr, eckit::DataBlobPtr blob);

protected:  // methods
    virtual void print(std::ostream&) const = 0;

    bool failOnError() { return failOnError_; }

private:  // methods
    friend std::ostream& operator<<(std::ostream& s, const DataSink& p) {
        p.print(s);
        return s;
    }

protected:  // members
    bool failOnError_;

    const eckit::LocalConfiguration config_;
    int id_;
};

//--------------------------------------------------------------------------------------------------

class DataSinkBuilderBase;

class DataSinkFactory : private eckit::NonCopyable {
private:  // methods
    DataSinkFactory() {}

public:  // methods
    static DataSinkFactory& instance();

    void add(const std::string& name, const DataSinkBuilderBase* builder);

    void remove(const std::string& name);

    void list(std::ostream&);

    DataSink* build(const std::string&, const eckit::Configuration& config);

private:  // members
    std::map<std::string, const DataSinkBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class DataSinkBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual DataSink* make(const eckit::Configuration& config) const = 0;

protected:  // methods
    DataSinkBuilderBase(const std::string&);

    virtual ~DataSinkBuilderBase();

    std::string name_;
};

template <class T>
class DataSinkBuilder final : public DataSinkBuilderBase {
    DataSink* make(const eckit::Configuration& config) const override { return new T(config); }

public:
    DataSinkBuilder(const std::string& name) : DataSinkBuilderBase(name) {}
};

//--------------------------------------------------------------------------------------------------

}  // namespace multio

#endif
