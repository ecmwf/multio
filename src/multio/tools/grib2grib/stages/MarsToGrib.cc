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
/// @brief Standalone `MarsToGrib` stage implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/stages/MarsToGrib.h"

#include <string>
#include <unordered_set>

#include "eckit/exception/Exceptions.h"

#include "metkit/mars2grib/api/Mars2Grib.h"
#include "metkit/mars2grib/api/Mars2GribTestCaseGenerator.h"

#include "multio/tools/grib2grib/Sink.h"
#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace implementation {

const std::unordered_set<std::string>& supportedMars2gribOptions() {
    static const auto supported = std::unordered_set<std::string>{"applyChecks",
                                                                  "enableOverride",
                                                                  "enableBitsPerValueCompression",
                                                                  "normalizeMars",
                                                                  "normalizeMisc",
                                                                  "fixMarsGrid",
                                                                  "skipSection3",
                                                                  "allowDefaultTimeIncrement",
                                                                  "allowZeroLengthFsWindow",
                                                                  "allowNonEnumeratedPositiveIntegerTimespanHours",
                                                                  "allowRedundantTimeIncrement",
                                                                  "allowMissingTimespanForInstantProduct",
                                                                  "allowMissingTimespanForStatisticalProduct"};
    return supported;
}

eckit::LocalConfiguration validateMars2gribConfiguration(const eckit::LocalConfiguration& options) {
    eckit::LocalConfiguration validated{};

    if (!options.has("mars2grib-options")) {
        return validated;
    }

    if (!options.isSubConfiguration("mars2grib-options")) {
        throw eckit::BadValue("mars2grib option 'mars2grib-options' must be a configuration section", Here());
    }

    const auto rawEncoderConf = options.getSubConfiguration("mars2grib-options");
    for (const auto& key : rawEncoderConf.keys()) {
        if (supportedMars2gribOptions().find(key) == supportedMars2gribOptions().end()) {
            throw eckit::BadValue("Unsupported mars2grib option 'mars2grib-options." + key + "'", Here());
        }

        if (!rawEncoderConf.isBoolean(key)) {
            throw eckit::BadValue("mars2grib option 'mars2grib-options." + key + "' must be boolean", Here());
        }

        validated.set(key, rawEncoderConf.getBool(key));
    }

    return validated;
}

}  // namespace implementation

void validateMarsToGribContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }

    if (config.has("mars2grib-generate-testcases")) {
        (void)config.getBool("mars2grib-generate-testcases");
    }

    if (config.has("mars2grib-testcases-dir")) {
        (void)config.getString("mars2grib-testcases-dir");
    }

    (void)implementation::validateMars2gribConfiguration(config);

    if (config.has("mars2grib-generate-testcases") && config.getBool("mars2grib-generate-testcases")
        && !config.has("mars2grib-testcases-dir")) {
        throw eckit::BadValue("mars2grib option 'mars2grib-testcases-dir' is required when testcases are enabled",
                              Here());
    }
}

MarsToGribContext parseMarsToGribContext(const eckit::LocalConfiguration& config) {
    MarsToGribContext parsed;

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    parsed.encoderConfig = implementation::validateMars2gribConfiguration(config);

    return parsed;
}

void freeMarsToGribContext(MarsToGribContext& context) noexcept {
    (void)context;
}

MarsToGribResult runMarsToGribStage(const std::vector<double>& values, const eckit::LocalConfiguration& mars,
                                    const eckit::LocalConfiguration& misc, const MarsToGribContext& context,
                                    TestCaseFileSink* testCaseSink) noexcept {
    MarsToGribResult result;

    try {
        metkit::mars2grib::Mars2Grib encoder{context.encoderConfig};
        result.encoded = encoder.encode(values, mars, misc);
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = MarsToGribCode::EncodeFailed;
        return result;
    }

    if (testCaseSink != nullptr) {
        std::string testCase;
        try {
            metkit::mars2grib::Mars2GribTestCaseGenerator generator{context.encoderConfig};
            testCase = generator.generate(mars, misc) + "\n";
        }
        catch (...) {
            printTrappedErrorDisclaimer();
            result.testCaseGenerationFailed = true;
        }

        if (!result.testCaseGenerationFailed) {
            try {
                testCaseSink->write(testCase);
            }
            catch (...) {
                printTrappedErrorDisclaimer();
                result.testCaseWriteFailed = true;
            }
        }
    }

    result.outcome = MarsToGribCode::Valid;
    return result;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
