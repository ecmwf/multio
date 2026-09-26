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
/// @brief Temporary diagnostic hooks for the isolated `grib2grib` pipeline.

#include "multio/tools/grib-to-mtg2/stages/Hacks.h"

#include <string>

#include "metkit/codes/api/CodesAPI.h"

#include "multio/sink/DataSink.h"
#include "multio/tools/grib-to-mtg2/CodesHandleToEckitMessage.h"
#include "multio/tools/grib-to-mtg2/Utils.h"

namespace multio::grib_to_mtg2 {

void runHacksStage(multio::sink::DataSink* sink, const metkit::codes::CodesHandle& inputGribMessage,
                   const metkit::codes::CodesHandle& outputGribMessage) noexcept {

    // Hack stage needs to be disabled in production
    return;

    if (sink == nullptr) {
        return;
    }

    try {
        if (outputGribMessage.getLong("param") != 152 || outputGribMessage.getString("levtype") != "ml"
            || outputGribMessage.getLong("levelist") != 100000) {
            return;
        }

        auto diagnosticInput = inputGribMessage.clone();
        diagnosticInput->set("expver", std::string{"2251"});
        sink->write(to_eckit_message(*diagnosticInput));
    }
    catch (...) {
        printTrappedErrorDisclaimer();
    }
}

}  // namespace multio::grib_to_mtg2
