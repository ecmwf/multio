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
#include "multio/util/Substitution.h"

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

LocalConfiguration rootConfig(const LocalConfiguration& config, const std::string& planName) {
    const auto actions
        = config.has("actions") ? config.getSubConfigurations("actions") : std::vector<LocalConfiguration>{};

    if (actions.empty()) {
        throw eckit::UserError("Plan config must define at least one action. Plan: " + planName);
    }

    return createActionList(actions);
}

const util::YAMLFile* getPlanConfiguration(const ConfigurationContext& confCtx) {
    ASSERT(confCtx.componentTag() == util::ComponentTag::Plan);
    if (confCtx.config().has("file")) {
        return &confCtx.getYAMLFile(confCtx.replaceCurly(confCtx.config().getString("file")));
    }
    return NULL;
}

}  // namespace

Plan::Plan(const ConfigurationContext& confCtx, const util::YAMLFile* file) :
    FailureAware(file ? confCtx.recast(file->content, confCtx.componentTag()) : confCtx) {
    name_ = (file && file->content.has("name"))
              ? file->content.getString("name")
              : (confCtx.config().has("name") ? confCtx.config().getString("name")
                                              : (file ? file->path.asString() : "anonymous"));
    auto tmp = util::parseEnabled((file) ? file->content : confCtx.config(), true);
    if (tmp) {
        enabled_ = *tmp;
    }
    else {
        throw eckit::UserError("Bool expected", Here());
    };
    auto root = rootConfig(file ? file->content : confCtx.config(), name_);
    root_ = ActionFactory::instance().build(root.getString("type"), confCtx.recast(root, util::ComponentTag::Action));
}

Plan::Plan(const ConfigurationContext& confCtx) : Plan(confCtx, getPlanConfiguration(confCtx)) {}

Plan::~Plan() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};
    logFile << "\n ** Plan " << name_ << " -- total wall-clock time spent processing: " << timing_ << "s" << std::endl;
}

void Plan::process(message::Message msg) {
    util::ScopedTimer timer{timing_};
    if (enabled_) {
        withFailureHandling([this, &msg]() { root_->execute(std::move(msg)); },
        // For failure handling a copy of the message needs to be captured... Note than the move above happens after the lambdas are initiated
                            [this, msg]() {
                                std::ostringstream oss;
                                oss << "Plan \"" << name_ << "\" with Message: " << msg << std::endl;
                                return oss.str();
                            });
    }
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

}  // namespace action
}  // namespace multio
