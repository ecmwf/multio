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

#include "multio/DataSink.h"

#include <mutex>

#include "eckit/exception/Exceptions.h"
#include "eckit/parser/JSON.h"
#include "eckit/value/Value.h"

#include "multio/LibMultio.h"


namespace multio {

using eckit::Configuration;
using eckit::Log;

//--------------------------------------------------------------------------------------------------

DataSinkFactory& DataSinkFactory::instance() {
    static DataSinkFactory singleton;
    return singleton;
}

void DataSinkFactory::add(const std::string& name, const DataSinkBuilderBase* builder) {
    std::lock_guard<std::mutex> lock{mutex_};
    ASSERT(factories_.find(name) == factories_.end());
    factories_[name] = builder;
}

void DataSinkFactory::remove(const std::string& name) {
    std::lock_guard<std::mutex> lock{mutex_};
    ASSERT(factories_.find(name) != factories_.end());
    factories_.erase(name);
}

void DataSinkFactory::list(std::ostream& out) {
    std::lock_guard<std::mutex> lock{mutex_};

    const char* sep = "";
    for (auto const& sinkFactory : factories_) {
        out << sep << sinkFactory.first;
        sep = ", ";
    }
}

DataSink* DataSinkFactory::build(const std::string& name, const Configuration& config) {
    std::lock_guard<std::mutex> lock{mutex_};

    Log::debug<LibMultio>() << "Looking for DataSinkFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end())
        return f->second->make(config);

    Log::error() << "No DataSinkFactory for [" << name << "]" << std::endl;
    Log::error() << "DataSinkFactories are:" << std::endl;
    for (auto const& factory : factories_) {
        Log::error() << "   " << factory.first << std::endl;
    }
    throw eckit::SeriousBug(std::string("No DataSinkFactory called ") + name);
}


DataSinkBuilderBase::DataSinkBuilderBase(const std::string& name) : name_(name) {
    DataSinkFactory::instance().add(name, this);
}

DataSinkBuilderBase::~DataSinkBuilderBase() {
    DataSinkFactory::instance().remove(name_);
}

//----------------------------------------------------------------------------------------------------------------------

DataSink::DataSink(const Configuration& config) :
    failOnError_(config.getBool("failOnError", true)),
    config_(config),
    id_(-1) {}

bool DataSink::ready() const {
    return true;  // default for synchronous sinks
}

eckit::Value DataSink::configValue() const {
    return config_.get();
}

void DataSink::flush() {}

void DataSink::setId(int id) {
    id_ = id;
}

int DataSink::id() const {
    return id_;
}

//----------------------------------------------------------------------------------------------------------------------

void DataSink::iopenfdb(const std::string& name, int& fdbaddr, const std::string& mode) {}

void DataSink::iclosefdb(int fdbaddr) {}

void DataSink::iinitfdb() {}

void DataSink::isetcommfdb(int rank) {}

void DataSink::isetrankfdb(int fdbaddr, int rank) {}

void DataSink::iset_fdb_root(int fdbaddr, const std::string& name) {}

void DataSink::iflushfdb(int fdbaddr) {}

void DataSink::isetfieldcountfdb(int fdbaddr, int all_ranks, int this_rank) {}

void DataSink::isetvalfdb(int fdbaddr, const std::string& name, const std::string& value) {}

void DataSink::iwritefdb(int fdbaddr, eckit::DataBlobPtr blob) {
    std::ostringstream msg;
    msg << "DataSink::iwritefdb() not implemented in derived class";
    throw eckit::SeriousBug(msg.str());
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio
