/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_server_MultioClient_H
#define multio_server_MultioClient_H

#include <memory>
#include <vector>
#include <map>

#include "multio/action/Plan.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/FailureHandling.h"

#include "eckit/log/Statistics.h"

namespace eckit {
class Buffer;
class Configuration;
class LocalConfiguration;
}  // namespace eckit

namespace multio {

using util::ConfigurationContext;
using util::ClientConfigurationContext;
using util::ComponentTag;

namespace message {
class Message;
class Metadata;
}

namespace server {

class Transport;

class MultioClient: public util::FailureAware<util::ComponentTag::Client> {
public:
    explicit MultioClient(const ClientConfigurationContext& config);

    ~MultioClient();

    void openConnections();
    void closeConnections();

    void dispatch(message::Metadata metadata, eckit::Buffer&& payload, message::Message::Tag tag);

    void dispatch(message::Message msg);
    
    util::FailureHandlerResponse handleFailure(const eckit::Optional<util::OnClientError>&) override;

private:
    std::vector<std::unique_ptr<action::Plan>> plans_;

    eckit::Timing totClientTiming_;
    eckit::Timer totClientTimer_;
};

}  // namespace server
}  // namespace multio

#endif
