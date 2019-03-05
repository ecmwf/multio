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

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "sandbox/Action.h"

using eckit::LocalConfiguration;

namespace multio {
namespace sandbox {

Plan::Plan(const eckit::Configuration& config) {
    name_ = config.getString("name", "anonymous");

    if (not config.has("actions")) {
        throw eckit::UserError("Plan config must define 'actions'");
    }

    const LocalConfiguration actions = config.getSubConfiguration("actions");
    if (not actions.has("root")) {
        throw eckit::UserError("Plan actions must define 'root' action");
    }

    const LocalConfiguration root = actions.getSubConfiguration("root");
    root_.reset(ActionFactory::instance().build(root.getString("type"), root));
}

Plan::~Plan() = default;

void Plan::process(Message msg) {
    root_->execute(msg);
}

}  // namespace sandbox
}  // namespace multio
