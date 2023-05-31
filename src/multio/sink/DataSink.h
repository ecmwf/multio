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

#pragma once

#include <iosfwd>
#include <map>
#include <mutex>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/message/Message.h"
#include "multio/config/ComponentConfiguration.h"

namespace multio::sink {

//----------------------------------------------------------------------------------------------------------------------

class DataSink {
public:  // methods
    DataSink(const config::ComponentConfiguration& compConf);

    virtual ~DataSink();

    virtual bool ready() const;

    virtual void write(eckit::message::Message message) = 0;

    /// No further writes to this sink
    virtual void flush();

    /// Set the datasink ID that is used by other classes to identify this one.
    /// In particular, it labels which sink within a MultIO this one is.
    void setId(int id);
    int id() const;

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

    const config::ComponentConfiguration compConf_;
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

    std::unique_ptr<DataSink> build(const std::string&, const config::ComponentConfiguration& compConf);

private:  // members
    std::map<std::string, const DataSinkBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class DataSinkBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::unique_ptr<DataSink> make(const config::ComponentConfiguration& compConf) const = 0;

protected:  // methods
    DataSinkBuilderBase(const std::string&);

    virtual ~DataSinkBuilderBase();

    std::string name_;
};

template <class T>
class DataSinkBuilder final : public DataSinkBuilderBase {
    std::unique_ptr<DataSink> make(const config::ComponentConfiguration& compConf) const override {
        return std::make_unique<T>(compConf);
    }

public:
    DataSinkBuilder(const std::string& name) : DataSinkBuilderBase(name) {}
};

//--------------------------------------------------------------------------------------------------

}  // namespace multio::sink
