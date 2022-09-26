/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Plan.h"

#include <fstream>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"
#include "multio/action/Action.h"
#include "multio/util/ScopedTimer.h"
#include "multio/util/logfile_name.h"

using eckit::LocalConfiguration;

namespace multio {
namespace action {

namespace {
LocalConfiguration createActionList(std::vector<LocalConfiguration> actions) {
    auto rit = actions.rbegin();
    auto current = *rit++;
    while (rit != actions.rend()) {
        eckit::Log::debug<LibMultio>() << " *** Current configuration: " << current << std::endl;
        auto parent = *rit++;
        parent.set("next", current);
        current = parent;
    }

    return current;
}

LocalConfiguration rootConfig(const LocalConfiguration& config) {
    const auto actions = config.has("actions") ? config.getSubConfigurations("actions")
                                               : std::vector<LocalConfiguration>{};

    if (actions.empty()) {
        throw eckit::UserError("Plan config must define at least one action");
    }

    return createActionList(actions);
}

}  // namespace

Plan::Plan(const ConfigurationContext& confCtx) {
    ASSERT(confCtx.componentTag() == util::ComponentTag::Plan);
    name_ = confCtx.config().getString("name", "anonymous");
    auto root = rootConfig(confCtx.config());
    root_.reset(ActionFactory::instance().build(root.getString("type"),
                                                confCtx.recast(root, util::ComponentTag::Action)));
}

Plan::~Plan() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};
    logFile << "\n ** Plan " << name_ << " -- total wall-clock time spent processing: " << timing_
            << "s" << std::endl;
}

void Plan::process(message::Message msg) {
    util::ScopedTimer timer{timing_};
    root_->execute(std::move(msg));
}

void Plan::computeActiveFields(std::insert_iterator<std::set<std::string>>& ins) const {
    root_->computeActiveFields(ins);
};

void Plan::computeActiveCategories(std::insert_iterator<std::set<std::string>>& ins) const {
    root_->computeActiveCategories(ins);
};

// ClientPlan
ClientPlan::ClientPlan(const ConfigurationContext& confCtx): Plan(confCtx) {}

std::shared_ptr<transport::Transport> ClientPlan::getTransport() const {
    auto sp = root_->getTransport().lock();
    if (!sp) {
        throw eckit::Exception("Client plan \"" + name_ + "\" has no transport.");
    }
    return sp; 
}

void ClientPlan::process(message::Message msg) {
    switch(msg.tag()) {
        case message::Message::Tag::Domain:
        case message::Message::Tag::Field:
        case message::Message::Tag::Mask:
        case message::Message::Tag::StepComplete:
        case message::Message::Tag::StepNotification: {
            auto md = msg.metadata();
            md.set("domainCount", getTransport()->clientCount());
            msg = msg.modifyMetadata(std::move(md));
            break;
        }
        default:
            break;
    }
    Plan::process(std::move(msg));
}


}  // namespace action
}  // namespace multio
