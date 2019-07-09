/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_server_Listener_H
#define multio_server_Listener_H

#include "Dispatcher.h"

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/log/Log.h"

#include "multio/server/Plan.h"

using eckit::LocalConfiguration;

namespace multio {
namespace server {

Dispatcher::Dispatcher(const eckit::Configuration& config) {
    eckit::Log::info() << config << std::endl;

    const std::vector<LocalConfiguration> plans = config.getSubConfigurations("plans");
    for (const auto& cfg : plans) {
        eckit::Log::info() << cfg << std::endl;
        plans_.emplace_back(new Plan(cfg));
    }
}

Dispatcher::~Dispatcher() = default;

void Dispatcher::dispatch(eckit::Queue<Message>& queue) {
    Message msg;
    while (queue.pop(msg) >= 0) {
        for (const auto& plan : plans_) {
            plan->process(msg);
        }
    }
}

}  // namespace server
}  // namespace multio

#endif
