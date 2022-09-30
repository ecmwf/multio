/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#ifndef multio_server_Dispatcher_H
#define multio_server_Dispatcher_H

#include <memory>
#include <atomic>

#include "eckit/container/Queue.h"
#include "eckit/log/Statistics.h"
#include "eckit/memory/NonCopyable.h"
#include "multio/util/FailureHandling.h"

#include "multio/message/Message.h"
#include "multio/util/ConfigurationContext.h"

namespace eckit {
class Configuration;
class Timer;
}  // namespace eckit

namespace multio {

namespace action {
class Plan;
}

namespace server {

class Dispatcher : public util::FailureAware<util::ComponentTag::Dispatcher>, private eckit::NonCopyable {
public:
    Dispatcher(const util::ConfigurationContext& confCtx, std::shared_ptr<std::atomic<bool>> cont);
    ~Dispatcher();

    void dispatch(eckit::Queue<message::Message>& queue);
    
    util::FailureHandlerResponse handleFailure(util::OnDispatchError) const override;

private:

    void handle(const message::Message& msg) const;

    std::shared_ptr<std::atomic<bool>> continue_;
    std::vector<std::unique_ptr<action::Plan>> plans_;

    eckit::Timing timing_;
    eckit::Timer timer_;
};

}  // namespace server
}  // namespace multio

#endif
