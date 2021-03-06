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

namespace eckit {
class Configuration;
}  // namespace eckit

namespace multio {
namespace server {

class Transport;

class MultioServer {
public:
    MultioServer(const eckit::Configuration& config);

    ~MultioServer();

private:
    std::shared_ptr<Transport> transport_ = nullptr;
    Listener listener_;
};
}  // namespace server
}  // namespace multio

#endif
