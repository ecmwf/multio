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

namespace eckit {
class Buffer;
class Configuration;
class LocalConfiguration;
}  // namespace eckit

namespace multio {

namespace message {
class Message;
using Metadata = eckit::LocalConfiguration;
}

namespace server {

class Transport;

class MultioClient {
public:
    explicit MultioClient(const eckit::Configuration& config);

    ~MultioClient();

    void openConnections();
    void closeConnections();

    void dispatch(message::Metadata metadata, eckit::Buffer&& payload, int itag);

    void dispatch(message::Message msg);

private:
    std::vector<std::unique_ptr<action::Plan>> plans_;
};

}  // namespace server
}  // namespace multio

#endif
