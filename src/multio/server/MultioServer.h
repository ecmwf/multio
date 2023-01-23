/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_server_MultioServer_H
#define multio_server_MultioServer_H

#include <memory>

#include "multio/server/Listener.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/FailureHandling.h"

namespace eckit {
class Configuration;
}  // namespace eckit

namespace multio {

using util::ServerConfigurationContext;
using util::FailureAware;

namespace transport {
class Transport;
}

namespace server {

class MultioServer: FailureAware<util::ComponentTag::Server> {
public:
    MultioServer(const ServerConfigurationContext& confCtx);
    
    util::FailureHandlerResponse handleFailure(util::OnServerError, const util::FailureContext&, util::DefaultFailureState&) const override;

    ~MultioServer();

private:
    std::shared_ptr<transport::Transport> transport_ = nullptr;
    Listener listener_;
};
}  // namespace server
}  // namespace multio

#endif
