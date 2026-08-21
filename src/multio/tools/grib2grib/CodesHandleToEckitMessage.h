/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Conversion bridge from `metkit::codes::CodesHandle` to `eckit::message::Message`.

#pragma once

#include "eckit/message/Message.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Copy one GRIB message out of a `CodesHandle` into an owning eckit message.
/// @param handle Read-only ecCodes wrapper whose current encoded message will be copied.
/// @return Owning `eckit::message::Message` whose payload remains valid independently of `handle`.
/// @throw eckit exception If the message cannot be copied into the destination buffer.
eckit::message::Message to_eckit_message(const metkit::codes::CodesHandle& handle);

}  // namespace multio::distGrib1ToGrib2::grib2grib
