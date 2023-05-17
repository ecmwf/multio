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
#include "multio/message/MetadataSelector.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/util/FailureHandling.h"

#include "eckit/log/Statistics.h"

namespace eckit {
class Buffer;
class Configuration;
class LocalConfiguration;
}  // namespace eckit

namespace multio {

using config::ClientConfiguration;
using config::ComponentTag;
using config::ComponentConfiguration;

namespace message {
class Message;
class Metadata;
}  // namespace message

namespace server {

class Transport;

class MultioClient : public util::FailureAware<config::ComponentTag::Client> {
public:
    explicit MultioClient(const ClientConfiguration& config);

    ~MultioClient();

    void openConnections();
    void closeConnections();

    void dispatch(message::Metadata metadata, eckit::Buffer&& payload, message::Message::Tag tag);

    void dispatch(message::Message msg);

    bool isFieldMatched(const message::Metadata& matcher) const;

    util::FailureHandlerResponse handleFailure(util::OnClientError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

private:
    std::vector<std::unique_ptr<action::Plan>> plans_;
    message::MetadataSelectors activeSelectors_;

    eckit::Timing totClientTiming_;
    eckit::Timer totClientTimer_;
};

}  // namespace server
}  // namespace multio
