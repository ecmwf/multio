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

#include "multio/tools/utils/distGrib1ToGrib2Logging.h"
#include "multio/tools/utils/grib2MarsMisc.h"

namespace eckit::message {
class Message;
}

namespace metkit::mars2grib {
class Mars2Grib;
}

namespace multio::sink {
class DataSink;
}

namespace multio::distGrib1ToGrib2 {

void processOneMessage(const eckit::message::Message& msg, const grib2MarsMisc::Grib2MarsMiscOptions& options,
                       metkit::mars2grib::Mars2Grib& encoder, sink::DataSink& writer, FileOutcome& outcome);

}  // namespace multio::distGrib1ToGrib2