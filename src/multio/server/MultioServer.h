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

namespace eckit {
class Configuration;
}  // namespace eckit

namespace multio {

using util::ServerConfigurationContext;

namespace transport {
class Transport;
}

namespace server {

class MultioServer {
public:
    MultioServer(const ServerConfigurationContext& confCtx);

    ~MultioServer();

private:
    std::shared_ptr<transport::Transport> transport_ = nullptr;
    Listener listener_;
};
}  // namespace server
}  // namespace multio

#endif
