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

#include "multio/library/LibMultio.h"
#include "multio/action/Plan.h"

#include "multio/util/ScopedTimer.h"

using eckit::LocalConfiguration;

namespace multio {
namespace server {

Dispatcher::Dispatcher(const eckit::Configuration& config) {
    timer_.start();

    eckit::Log::debug<LibMultio>() << config << std::endl;

    const std::vector<LocalConfiguration> plans = config.getSubConfigurations("plans");
    for (const auto& cfg : plans) {
        eckit::Log::debug<LibMultio>() << cfg << std::endl;
        plans_.emplace_back(new action::Plan(cfg));
    }
}

Dispatcher::~Dispatcher() {
    eckit::Log::info() << " ******* Total wall-clock time spent in dispatcher "
                       << eckit::Timing{timer_}.elapsed_ << "s" << std::endl
                       << "         Total wall-clock time spent with dispatching " << timing_ << "s"
                       << std::endl;
}

void Dispatcher::dispatch(eckit::Queue<message::Message>& queue) {
    ScopedTimer timer{timing_};
    message::Message msg;
    while (queue.pop(msg) >= 0) {
        for (const auto& plan : plans_) {
            plan->process(msg);
        }
    }
}

}  // namespace server
}  // namespace multio

#endif
