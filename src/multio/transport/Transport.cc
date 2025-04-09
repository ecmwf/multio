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
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"

namespace multio::transport {

using eckit::Configuration;
using eckit::Log;

//--------------------------------------------------------------------------------------------------

namespace {

std::string transportExceptionReason(const std::string& r) {
    std::string s("Transport exception: ");
    s.append(r);
    return s;
}

}  // namespace

TransportException::TransportException(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(transportExceptionReason(r), l) {}

//--------------------------------------------------------------------------------------------------

Transport::Transport(const ComponentConfiguration& compConf) : compConf_{compConf} {
    LOG_DEBUG_LIB(LibMultio) << "Transport config: " << compConf.parsedConfig() << std::endl;
}

Transport::~Transport() = default;

void Transport::listen() {}

const PeerList& Transport::clientPeers() const {
    if (peersMissing()) {
        createPeers();
    }
    return clientPeers_;
}

const PeerList& Transport::serverPeers() const {
    if (peersMissing()) {
        createPeers();
    }
    return serverPeers_;
}

bool Transport::peersMissing() const {
    return clientPeers_.empty() && serverPeers_.empty();
}

size_t Transport::clientCount() const {
    if (peersMissing()) {
        createPeers();
    }
    return clientPeers_.size();
}

size_t Transport::serverCount() const {
    if (peersMissing()) {
        createPeers();
    }
    return serverPeers_.size();
}

//--------------------------------------------------------------------------------------------------

TransportFactory& TransportFactory::instance() {
    static TransportFactory singleton;
    return singleton;
}

void TransportFactory::add(const std::string& name, const TransportBuilderBase* builder) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) == factories_.end());
    LOG_DEBUG_LIB(LibMultio) << "Adding TransportFactory [" << name << "]" << std::endl;
    factories_[name] = builder;
}

void TransportFactory::remove(const std::string& name) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) != factories_.end());
    factories_.erase(name);
}

void TransportFactory::list(std::ostream& out) const {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    const char* sep = "";
    for (auto const& sinkFactory : factories_) {
        out << sep << sinkFactory.first;
        sep = ", ";
    }
}

std::unique_ptr<Transport> TransportFactory::build(const std::string& name, const ComponentConfiguration& compConf) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    Log::debug<LibMultio>() << "Looking for TransportFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end()) {
        return f->second->make(compConf);
    }

    Log::error() << "No TransportFactory for [" << name << "]" << std::endl;
    Log::error() << "TransportFactories are:" << std::endl;
    for (auto const& factory : factories_) {
        Log::error() << "   " << factory.first << std::endl;
    }
    throw TransportException(std::string("No TransportFactory called ") + name, Here());
}


TransportBuilderBase::TransportBuilderBase(const std::string& name) : name_(name) {
    TransportFactory::instance().add(name, this);
}

TransportBuilderBase::~TransportBuilderBase() {
    TransportFactory::instance().remove(name_);
}

//--------------------------------------------------------------------------------------------------

}  // namespace multio::transport
