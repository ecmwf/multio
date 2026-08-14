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
/// @brief Standalone `PostEncodeValidation` stage implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/stages/PostEncodeValidation.h"

#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

void validatePostEncodeValidationContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }
}

PostEncodeValidationContext parsePostEncodeValidationContext(const eckit::LocalConfiguration& config) {
    PostEncodeValidationContext parsed;

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    return parsed;
}

void freePostEncodeValidationContext(PostEncodeValidationContext& context) noexcept {
    (void)context;
}

PostEncodeValidationCode runPostEncodeValidationStage(const metkit::codes::CodesHandle& encodedHandle,
                                                      const PostEncodeValidationContext& context) noexcept {
    (void)context;

    try {
        return encodedHandle.getLong("isMessageValid") == 1 ? PostEncodeValidationCode::Valid
                                                             : PostEncodeValidationCode::InvalidEncodedMessage;
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        return PostEncodeValidationCode::InvalidEncodedMessage;
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
