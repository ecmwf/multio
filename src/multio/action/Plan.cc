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
#include "multio/config/PlanConfiguration.h"
#include "multio/util/ScopedTimer.h"
#include "multio/util/Substitution.h"
#include "multio/util/logfile_name.h"

using eckit::LocalConfiguration;

namespace multio::action {

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

LocalConfiguration rootConfig(const LocalConfiguration& config, const std::string& planName) {
    const auto actions
        = config.has("actions") ? config.getSubConfigurations("actions") : std::vector<LocalConfiguration>{};

    if (actions.empty()) {
        throw eckit::UserError("Plan config must define at least one action. Plan: " + planName);
    }

    return createActionList(actions);
}

}  // namespace


std::vector<std::unique_ptr<action::Plan>> Plan::makePlans(
    const std::vector<eckit::LocalConfiguration>& componentConfig, const config::MultioConfiguration& multioConf,
    message::MetadataSelectors& selectors) {

    std::vector<std::unique_ptr<action::Plan>> plans;

    // Create the array of plans
    LOG_DEBUG_LIB(multio::LibMultio) << "make_plans: " << componentConfig << std::endl;
    for (auto&& cfg : componentConfig) {
        auto planCfgs = configurePlans(cfg, multioConf);
        // Works also for empty planCfgs
        for (auto& pcfg : planCfgs) {
            LOG_DEBUG_LIB(multio::LibMultio) << pcfg << std::endl;
            plans.emplace_back(std::make_unique<action::Plan>(ComponentConfiguration(pcfg, multioConf)))
                ->matchedFields(selectors);
        }
    }

    // Exit point
    return plans;
};


std::vector<std::unique_ptr<action::Plan>> Plan::makePlans(
    const std::vector<eckit::LocalConfiguration>& componentConfig, const config::MultioConfiguration& multioConf) {

    std::vector<std::unique_ptr<action::Plan>> plans;

    // Create the array of plans
    LOG_DEBUG_LIB(multio::LibMultio) << "make_plans: " << componentConfig << std::endl;
    for (auto&& cfg : componentConfig) {
        auto planCfgs = configurePlans(cfg, multioConf);
        // Works also for empty planCfgs
        for (auto& pcfg : planCfgs) {
            LOG_DEBUG_LIB(multio::LibMultio) << pcfg << std::endl;
            plans.emplace_back(std::make_unique<action::Plan>(ComponentConfiguration(pcfg, multioConf)));
        }
    }

    // Exit point
    return plans;
};


Plan::Plan(const ComponentConfiguration& compConf) :
    FailureAware(compConf),
    name_{compConf.parsedConfig().getString("name")},
    root_{ActionFactory::instance().build(
        rootConfig(compConf.parsedConfig(), name_).getString("type"),
        ComponentConfiguration(rootConfig(compConf.parsedConfig(), name_), compConf.multioConfig()))} {}

Plan::~Plan() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};
    logFile << "\n ** Plan " << name_ << " -- total wall-clock time spent processing: " << timing_ << "s" << std::endl;
}

void Plan::process(message::Message msg) {
    util::ScopedTimer timer{timing_};
    withFailureHandling([this, &msg]() { root_->execute(std::move(msg)); },
                        // For failure handling a copy of the message needs to be captured... Note than the move
                        // above happens after the lambdas are initiated
                        [this, msg]() {
                            std::ostringstream oss;
                            oss << "Plan \"" << name_ << "\" with Message: " << msg << std::endl;
                            return oss.str();
                        });
}

util::FailureHandlerResponse Plan::handleFailure(util::OnPlanError t, const util::FailureContext&,
                                                 util::DefaultFailureState&) const {
    if (t == util::OnPlanError::Recover) {
        return util::FailureHandlerResponse::Retry;
    }
    return util::FailureHandlerResponse::Rethrow;
};


void Plan::matchedFields(message::MetadataSelectors& selectors) const {
    root_->matchedFields(selectors);
}

}  // namespace multio::action
