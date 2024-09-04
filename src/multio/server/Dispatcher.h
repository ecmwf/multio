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

#include <atomic>
#include <memory>

#include "eckit/container/Queue.h"
#include "eckit/log/Statistics.h"
#include "eckit/memory/NonCopyable.h"
#include "multio/util/FailureHandling.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/util/Timing.h"

namespace eckit {
class Configuration;
class Timer;
}  // namespace eckit

namespace multio {

namespace action {
class Plan;
}

namespace server {

struct DispatcherFailureTraits {
    using OnErrorType = util::OnDispatchError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence = util::integer_sequence<OnErrorType, OnErrorType::Propagate>;
    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnErrorType::Propagate; };
    static inline std::string configKey() { return std::string("on-dispatch-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    };
    static inline std::string componentName() { return std::string("Dispatcher"); };
};


class Dispatcher : public util::FailureAware<DispatcherFailureTraits>, private eckit::NonCopyable {
public:
    Dispatcher(const config::ComponentConfiguration& compConf, eckit::Queue<message::Message>& queue);
    ~Dispatcher();

    void dispatch();

    util::FailureHandlerResponse handleFailure(util::OnDispatchError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

private:
    void handle(message::Message msg) const;

    eckit::Queue<message::Message>& queue_;
    std::vector<std::unique_ptr<action::Plan>> plans_;

    util::Timing<> timing_;
};

}  // namespace server
}  // namespace multio
