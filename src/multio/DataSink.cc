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

#include "eckit/exception/Exceptions.h"
#include "eckit/parser/JSON.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

namespace {
using SinkFactoryRegister = std::map<std::string, const DataSinkFactory*>;

SinkFactoryRegister& sinkFactories() {
    static auto reg = new SinkFactoryRegister{};
    return *reg;
}
eckit::Mutex& local_mutex() {
    static auto mutex = new eckit::Mutex{};
    return *mutex;
}
}  // namespace

DataSinkFactory::DataSinkFactory(const std::string& name) : name_(name) {
    AutoLock<Mutex> lock{local_mutex()};

    ASSERT(sinkFactories().find(name) == sinkFactories().end());
    sinkFactories()[name] = this;
}


DataSinkFactory::~DataSinkFactory() {
    AutoLock<Mutex> lock{local_mutex()};
    sinkFactories().erase(name_);
}


void DataSinkFactory::list(std::ostream& out) {

    AutoLock<Mutex> lock{local_mutex()};

    const char* sep = "";
    for (auto const& sinkFactory : sinkFactories()) {
        out << sep << sinkFactory.first;
        sep = ", ";
    }
}


DataSink* DataSinkFactory::build(const std::string &name, const Configuration& config) {

    AutoLock<Mutex> lock{local_mutex()};

    Log::info() << "Looking for DataSinkFactory [" << name << "]" << std::endl;

    auto requiredFactory = sinkFactories().find(name);
    if (requiredFactory == sinkFactories().end()) {
        Log::error() << "No DataSinkFactory for [" << name << "]" << std::endl;
        Log::error() << "DataSinkFactories are:" << std::endl;
        for (auto const& sinkFactory : sinkFactories()) {
            Log::error() << "   " << sinkFactory.first << std::endl;
        }
        throw SeriousBug(std::string("No DataSinkFactory called ") + name);
    }

    return requiredFactory->second->make(config);
}

//----------------------------------------------------------------------------------------------------------------------

DataSink::DataSink(const Configuration& config) :
    failOnError_( config.getBool("failOnError",true) ),
    config_(config),
    id_(-1) {
}

DataSink::~DataSink() {
}

bool DataSink::ready() const
{
    return true; // default for synchronous sinks
}


Value DataSink::configValue() const {
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

void DataSink::iopenfdb(const std::string& name, int& fdbaddr, const std::string& mode) {
}

void DataSink::iclosefdb(int fdbaddr) {
}

void DataSink::iinitfdb() {
}

void DataSink::isetcommfdb(int rank) {
}

void DataSink::isetrankfdb(int fdbaddr, int rank) {
}

void DataSink::iset_fdb_root(int fdbaddr, const std::string& name) {
}

void DataSink::iflushfdb(int fdbaddr) {
}

void DataSink::isetfieldcountfdb(int fdbaddr,int all_ranks, int this_rank) {
}

void DataSink::isetvalfdb(int fdbaddr, const std::string& name, const std::string& value) {
}

void DataSink::iwritefdb(int fdbaddr, eckit::DataBlobPtr blob) {
    std::ostringstream msg;
    msg << "DataSink::iwritefdb() not implemented in derived class";
    throw SeriousBug(msg.str());
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

