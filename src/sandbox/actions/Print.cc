/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_sandbox_Listener_H
#define multio_sandbox_Listener_H

#include "Print.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/config/Configuration.h"

namespace multio {
namespace sandbox {
namespace actions {

Print::Print(const eckit::Configuration& config): Action(config) {

    auto stream = config.getString("stream", "info");

    if(stream == "info") {
        os = &eckit::Log::info();
    }
    else {
        os = &eckit::Log::error();
    }
}

void Print::execute(Message msg)
{
    ASSERT(os);
    (*os) << msg;
}

static ActionBuilder<Print> PrintBuilder("Print");

}  // namespace actions
}  // namespace sandbox
}  // namespace multio

#endif
