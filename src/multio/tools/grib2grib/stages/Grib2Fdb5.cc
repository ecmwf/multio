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
/// @brief Standalone `Grib2Fdb5` stage implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/stages/Grib2Fdb5.h"

#include "multio/sink/DataSink.h"

#include "multio/tools/grib2grib/CodesHandleToEckitMessage.h"

#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

void validateGrib2Fdb5Context(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }
}

Grib2Fdb5Context parseGrib2Fdb5Context(const eckit::LocalConfiguration& config) {
    Grib2Fdb5Context parsed;

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    return parsed;
}

void freeGrib2Fdb5Context(Grib2Fdb5Context& context) noexcept {
    (void)context;
}

Grib2Fdb5Result runGrib2Fdb5Stage(const metkit::codes::CodesHandle& encodedHandle, const Grib2Fdb5Context& context,
                                   multio::sink::DataSink* writer) noexcept {
    Grib2Fdb5Result result;

    (void)context;

    if (writer == nullptr) {
        result.outcome = Grib2Fdb5Code::Valid;
        return result;
    }

    try {
        writer->write(to_eckit_message(encodedHandle));
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = Grib2Fdb5Code::ArchiveFailed;
        return result;
    }

    result.outcome = Grib2Fdb5Code::Valid;
    return result;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
