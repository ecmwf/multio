/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "eckit/message/Message.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::tools::utils {

eckit::message::Message to_eckit_message(const metkit::codes::CodesHandle& handle);

}  // namespace multio::tools::utils
