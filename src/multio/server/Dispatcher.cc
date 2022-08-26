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

#include <fstream>

#include "eckit/config/LocalConfiguration.h"

#include "multio/LibMultio.h"
#include "multio/action/Plan.h"

#include "multio/domain/Mappings.h"
#include "multio/domain/Mask.h"

#include "multio/util/ScopedTimer.h"
#include "multio/util/logfile_name.h"

using eckit::LocalConfiguration;

namespace multio {
namespace server {

Dispatcher::Dispatcher(const util::ConfigurationContext& confCtx) {
    timer_.start();

    eckit::Log::debug<LibMultio>() << confCtx.config() << std::endl;

    util::ConfigurationContext::SubConfigurationContexts plans = confCtx.subContexts("plans");
    for (auto&& subCtx: plans) {
        eckit::Log::debug<LibMultio>() << subCtx.config() << std::endl;
        plans_.emplace_back(new action::Plan(std::move(subCtx)));
    }
}

Dispatcher::~Dispatcher() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};
    logFile << "\n ** Total wall-clock time spent in dispatcher " << eckit::Timing{timer_}.elapsed_
            << "s -- of which time spent with dispatching " << timing_ << "s" << std::endl;
}

void Dispatcher::dispatch(eckit::Queue<message::Message>& queue) {
    util::ScopedTimer timer{timing_};
    message::Message msg;
    auto sz = queue.pop(msg);
    while (sz >= 0) {
        handle(msg);
        LOG_DEBUG_LIB(multio::LibMultio) << "Size of the dispatch queue: " << sz << std::endl;
        sz = queue.pop(msg);
    }
}

void Dispatcher::handle(const message::Message& msg) const {
    switch (msg.tag()) {
        case message::Message::Tag::Domain:
            domain::Mappings::instance().add(msg);
            break;

        case message::Message::Tag::Mask:
            domain::Mask::instance().add(msg);
            break;

        default:
            for (const auto& plan : plans_) {
                plan->process(msg);
            }
    }
}

}  // namespace server
}  // namespace multio

#endif
