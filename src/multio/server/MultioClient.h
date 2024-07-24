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

#include <map>
#include <memory>
#include <vector>

#include "multio/action/Plan.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/message/Metadata.h"
#include "multio/message/MetadataMatcher.h"
#include "multio/util/FailureHandling.h"

#include "eckit/log/Statistics.h"

namespace eckit {
class Buffer;
class Configuration;
class LocalConfiguration;
}  // namespace eckit

namespace multio {

using config::MultioConfiguration;
using config::MultioConfigurationHolder;


namespace server {

struct ClientFailureTraits {
    using OnErrorType = util::OnClientError;
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
    static inline std::string componentName() { return std::string("Client"); };
};


class MultioClient : public MultioConfigurationHolder, public util::FailureAware<ClientFailureTraits> {
public:
    MultioClient();
    MultioClient(MultioConfiguration&& multioConf);

    ~MultioClient();

    void openConnections();
    void closeConnections();

    void dispatch(message::SharedMetadata metadata, eckit::Buffer&& payload, message::Message::Tag tag);
    void dispatch(message::SharedMetadata metadata, const message::PayloadReference& payload,
                  message::Message::Tag tag);

    void dispatch(message::Message msg);

    bool isFieldMatched(const message::Metadata& matcher) const;

    util::FailureHandlerResponse handleFailure(util::OnClientError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

private:
    MultioClient(const eckit::LocalConfiguration& conf, MultioConfiguration&& multioConf);

    std::vector<std::unique_ptr<action::Plan>> plans_;
    message::match::MatchReduce activeSelectors_{message::match::Reduce::Or};

    eckit::Timing totClientTiming_;
    eckit::Timer totClientTimer_;
};

}  // namespace server
}  // namespace multio
