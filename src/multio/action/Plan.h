/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <memory>
#include <optional>

#include "eckit/log/Statistics.h"
#include "eckit/memory/NonCopyable.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/util/FailureHandling.h"


namespace multio::message {
class MetadataSelectors;
}

namespace multio::action {

using config::ComponentConfiguration;
using util::FailureAware;

class Action;

class Plan : private eckit::NonCopyable, public FailureAware<config::ComponentTag::Plan> {
private:
    // Delegate constructor with loaded config (from file or list entry)
    Plan(std::tuple<ComponentConfiguration, std::string>&& confAndName);

public:
    Plan(const ComponentConfiguration& compConf);
    virtual ~Plan();

    virtual void process(message::Message msg);

    void matchedFields(message::MetadataSelectors& selectors) const;

    util::FailureHandlerResponse handleFailure(util::OnPlanError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

protected:
    bool enabled_;
    std::string name_;
    std::unique_ptr<Action> root_;
    eckit::Timing timing_;
};

}  // namespace multio::action
