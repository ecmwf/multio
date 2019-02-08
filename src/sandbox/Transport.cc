/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Transport.h"

#include <iostream>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"

namespace multio {
namespace sandbox {

using eckit::Configuration;
using eckit::Log;

//----------------------------------------------------------------------------------------------------------------------

TransportFactory& TransportFactory::instance() {
    static TransportFactory singleton;
    return singleton;
}

void TransportFactory::add(const std::string& name, const TransportBuilderBase* builder) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) == factories_.end());
    factories_[name] = builder;
}

void TransportFactory::remove(const std::string& name) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) != factories_.end());
    factories_.erase(name);
}

void TransportFactory::list(std::ostream& out) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    const char* sep = "";
    for (auto const& sinkFactory : factories_) {
        out << sep << sinkFactory.first;
        sep = ", ";
    }
}

Transport* TransportFactory::build(const std::string& name, const Configuration& config) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    Log::debug<LibMultio>() << "Looking for TransportFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end())
        return f->second->make(config);

    Log::error() << "No TransportFactory for [" << name << "]" << std::endl;
    Log::error() << "TransportFactories are:" << std::endl;
    for (auto const& factory : factories_) {
        Log::error() << "   " << factory.first << std::endl;
    }
    throw eckit::SeriousBug(std::string("No TransportFactory called ") + name);
}


TransportBuilderBase::TransportBuilderBase(const std::string& name) : name_(name) {
    TransportFactory::instance().add(name, this);
}

TransportBuilderBase::~TransportBuilderBase() {
    TransportFactory::instance().remove(name_);
}

//----------------------------------------------------------------------------------------------------------------------

Transport::Transport(){}

Transport::~Transport() = default;

//----------------------------------------------------------------------------------------------------------------------

}  // namespace sandbox
}  // namespace multio
