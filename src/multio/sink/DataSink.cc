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

#include "multio/sink/DataSink.h"

#include <mutex>

#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"

namespace multio::sink {

using eckit::Configuration;
using eckit::Log;

//--------------------------------------------------------------------------------------------------

DataSinkFactory& DataSinkFactory::instance() {
    static DataSinkFactory singleton;
    return singleton;
}

void DataSinkFactory::add(const std::string& name, const DataSinkBuilderBase* builder) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) == factories_.end());
    factories_[name] = builder;
}

void DataSinkFactory::remove(const std::string& name) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) != factories_.end());
    factories_.erase(name);
}

void DataSinkFactory::list(std::ostream& out) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    const char* sep = "";
    for (auto const& sinkFactory : factories_) {
        out << sep << sinkFactory.first;
        sep = ", ";
    }
}

std::unique_ptr<DataSink> DataSinkFactory::build(const std::string& name,
                                                 const config::ComponentConfiguration& compConf) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    LOG_DEBUG_LIB(LibMultio) << "Looking for DataSinkFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end())
        return f->second->make(compConf);

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

DataSink::DataSink(const config::ComponentConfiguration& compConf) :
    failOnError_(compConf.YAML().getBool("failOnError", true)), compConf_(compConf), id_(-1) {}

DataSink::~DataSink() {}

bool DataSink::ready() const {
    return true;  // default for synchronous sinks
}

void DataSink::flush() {}

void DataSink::setId(int id) {
    id_ = id;
}

int DataSink::id() const {
    return id_;
}

//----------------------------------------------------------------------------------------------------------------------

void DataSink::iopenfdb(const std::string&, int&, const std::string&) {}

void DataSink::iclosefdb(int) {}

void DataSink::iinitfdb() {}

void DataSink::isetcommfdb(int) {}

void DataSink::isetrankfdb(int, int) {}

void DataSink::iset_fdb_root(int, const std::string&) {}

void DataSink::iflushfdb(int) {}

void DataSink::isetfieldcountfdb(int, int, int) {}

void DataSink::isetvalfdb(int, const std::string&, const std::string&) {}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::sink
