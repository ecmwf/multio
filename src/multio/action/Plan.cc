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
    const auto actions =
        config.has("actions") ? config.getSubConfigurations("actions") : std::vector<LocalConfiguration>{};

    if (actions.empty()) {
        throw eckit::UserError("Plan config must define at least one action");
    }

    return createActionList(actions);
}

}  // namespace

Plan::Plan(const ConfigurationContext& confCtx) : FailureAware(confCtx) {
    ASSERT(confCtx.componentTag() == util::ComponentTag::Plan);
    name_ = confCtx.config().getString("name", "anonymous");
    enabled_ = confCtx.config().getBool("enable",true);
    auto root = rootConfig(confCtx.config());
    root_ = ActionFactory::instance().build(root.getString("type"), confCtx.recast(root, util::ComponentTag::Action));
}

Plan::~Plan() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};
    logFile << "\n ** Plan " << name_ << " -- total wall-clock time spent processing: " << timing_ << "s" << std::endl;
}

void Plan::process(message::Message msg) {
    util::ScopedTimer timer{timing_};
    if (enabled_) {
        withFailureHandling([&]() { root_->execute(std::move(msg)); }, [=]() {
            std::ostringstream oss;
            oss << "Plan \"" << name_ << "\" with Message: " << msg << std::endl;
            return oss.str();
        });
    }
}

util::FailureHandlerResponse Plan::handleFailure(util::OnPlanError t, const util::FailureContext&, util::DefaultFailureState&) const {
    if (t == util::OnPlanError::Recover) {
        return util::FailureHandlerResponse::Retry;
    }
    return util::FailureHandlerResponse::Rethrow;
};


void Plan::matchedFields(message::MetadataSelectors& selectors) const {
    root_->matchedFields(selectors);
}

}  // namespace action
}  // namespace multio
