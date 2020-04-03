/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_server_Listener_H
#define multio_server_Listener_H

#include "Action.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"

#include "multio/library/LibMultio.h"

using eckit::LocalConfiguration;

namespace multio {
namespace action {

namespace {

}  // namespace

using eckit::Configuration;
using eckit::Log;

//----------------------------------------------------------------------------------------------------------------------

Action::Action(const eckit::Configuration& config) : type_{config.getString("type")} {
    if (config.has("next")) {
        const LocalConfiguration next = config.getSubConfiguration("next");
        next_.reset(ActionFactory::instance().build(next.getString("type"), next));
    }
}

Action::~Action() {
    eckit::Log::info() << "         -- Total wall-clock time spent on action <" << type_
                       << ">: " << timing_.elapsed_ << "s" << std::endl;
}

void Action::execute(message::Message msg) const {
    if (doExecute(msg) && next_) {
        next_->execute(msg);
    }
}

//---------------------------------------------------------------------------------------------------------------

ActionFactory& ActionFactory::instance() {
    static ActionFactory singleton;
    return singleton;
}

void ActionFactory::add(const std::string& name, const ActionBuilderBase* builder) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) == factories_.end());
    factories_[name] = builder;
}

void ActionFactory::remove(const std::string& name) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) != factories_.end());
    factories_.erase(name);
}

void ActionFactory::list(std::ostream& out) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    const char* sep = "";
    for (auto const& sinkFactory : factories_) {
        out << sep << sinkFactory.first;
        sep = ", ";
    }
}

Action* ActionFactory::build(const std::string& name, const Configuration& config) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    Log::debug<LibMultio>() << "Looking for ActionFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end())
        return f->second->make(config);

    Log::error() << "No ActionFactory for [" << name << "]" << std::endl;
    Log::error() << "ActionFactories are:" << std::endl;
    for (auto const& factory : factories_) {
        Log::error() << "   " << factory.first << std::endl;
    }
    throw eckit::SeriousBug(std::string("No ActionFactory called ") + name);
}


ActionBuilderBase::ActionBuilderBase(const std::string& name) : name_(name) {
    ActionFactory::instance().add(name, this);
}

ActionBuilderBase::~ActionBuilderBase() {
    ActionFactory::instance().remove(name_);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

#endif
