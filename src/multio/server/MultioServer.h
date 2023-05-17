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

#include "multio/server/Listener.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/util/FailureHandling.h"

namespace eckit {
class Configuration;
}  // namespace eckit

namespace multio {

using util::FailureAware;
using config::ServerConfiguration;

namespace transport {
class Transport;
}

namespace server {

class MultioServer : FailureAware<config::ComponentTag::Server> {
public:
    MultioServer(const ServerConfiguration& compConf);

    util::FailureHandlerResponse handleFailure(util::OnServerError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

    ~MultioServer();

private:
    std::unique_ptr<transport::Transport> transport_;
    Listener listener_;
};
}  // namespace server
}  // namespace multio
