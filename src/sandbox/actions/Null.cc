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

#include "Null.h"

namespace multio {
namespace sandbox {
namespace actions {

Null::Null(const eckit::Configuration& config): Action(config) {
}

void Null::execute(Message msg)
{
    next_->execute(msg);
}

static ActionBuilder<Null> NullBuilder("Null");

}  // namespace actions
}  // namespace sandbox
}  // namespace multio

#endif
