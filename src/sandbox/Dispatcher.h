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

#ifndef multio_sandbox_Dispatcher_H
#define multio_sandbox_Dispatcher_H

#include <memory>

#include "eckit/container/Queue.h"
#include "eckit/memory/NonCopyable.h"

#include "sandbox/Message.h"

namespace eckit {
class Configuration;
}

namespace multio {
namespace sandbox {

class Plan;

class Dispatcher : private eckit::NonCopyable {
public:
    Dispatcher(const eckit::Configuration& config);
    ~Dispatcher();

    Dispatcher(Dispatcher&& rhs) = default;
    Dispatcher& operator=(Dispatcher&& rhs) = default;

    void dispatch(eckit::Queue<Message>& queue);

private:

    std::vector<std::unique_ptr<Plan>> plans_;

};

}  // namespace sandbox
}  // namespace multio

#endif
