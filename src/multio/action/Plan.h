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

#include "eckit/memory/NonCopyable.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/message/Message.h"
#include "multio/util/FailureHandling.h"
#include "multio/util/Timing.h"

namespace multio::message::match {
class MatchReduce;
}

namespace multio::action {

using config::ComponentConfiguration;
using util::FailureAware;

class Action;

struct PlanFailureTraits {
    using OnErrorType = util::OnPlanError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence = util::integer_sequence<OnErrorType, OnErrorType::Propagate, OnErrorType::Recover>;
    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnErrorType::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    };
    static inline std::string componentName() { return std::string("Plan"); };
};


class Plan : private eckit::NonCopyable, public FailureAware<PlanFailureTraits> {
public:
    Plan(const ComponentConfiguration& compConf);
    ~Plan();

    void process(message::Message msg);

    void matchedFields(message::match::MatchReduce& selectors) const;

    util::FailureHandlerResponse handleFailure(util::OnPlanError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

    static std::vector<std::unique_ptr<action::Plan>> makePlans(
        const std::vector<eckit::LocalConfiguration>& componentConfig, const config::MultioConfiguration& multioConf);

    static std::vector<std::unique_ptr<action::Plan>> makePlans(
        const std::vector<eckit::LocalConfiguration>& componentConfig, const config::MultioConfiguration& multioConf,
        message::match::MatchReduce& selectors);

    const std::string& name() const noexcept;

protected:
    const std::string name_;
    const std::unique_ptr<Action> root_;
    util::Timing<> timing_;
};


}  // namespace multio::action
