/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <memory>

#include "multio/config/ComponentConfiguration.h"
#include "multio/server/Listener.h"
#include "multio/util/FailureHandling.h"

namespace eckit {
class LocalConfiguration;
}  // namespace eckit

namespace multio {

using config::MultioConfiguration;
using config::MultioConfigurationHolder;
using util::FailureAware;

namespace transport {
class Transport;
}

namespace server {

struct ServerFailureTraits {
    using OnErrorType = util::OnServerError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence = util::integer_sequence<OnErrorType, OnErrorType::Propagate, OnErrorType::Recover,
                                               OnErrorType::AbortTransport>;
    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnErrorType::Propagate; };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline std::string componentName() { return std::string("Server"); };
};


class MultioServer : public MultioConfigurationHolder, public FailureAware<ServerFailureTraits> {
public:
    MultioServer(MultioConfiguration&& multioConf);
    ~MultioServer();

    util::FailureHandlerResponse handleFailure(util::OnServerError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

private:
    MultioServer(const eckit::LocalConfiguration& conf, MultioConfiguration&& multioConf);

    std::unique_ptr<transport::Transport> transport_;
    Listener listener_;
};
}  // namespace server
}  // namespace multio
