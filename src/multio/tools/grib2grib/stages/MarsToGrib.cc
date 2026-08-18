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

std::optional<eckit::LocalConfiguration> parseMars2GribApiOptions(const eckit::LocalConfiguration& config) {
    if (!config.has("api-options")) {
        return std::nullopt;
    }

    if (!config.isSubConfiguration("api-options")) {
        throw eckit::BadValue("mars-to-grib option 'api-options' must be a configuration section", Here());
    }

    return config.getSubConfiguration("api-options");
}

}  // namespace implementation

void validateMarsToGribContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }

    if (config.has("generate-testcases")) {
        (void)config.getBool("generate-testcases");
    }

    if (config.has("testcases-dir")) {
        (void)config.getString("testcases-dir");
    }

    (void)implementation::parseMars2GribApiOptions(config);

    if (config.has("generate-testcases") && config.getBool("generate-testcases") && !config.has("testcases-dir")) {
        throw eckit::BadValue("mars-to-grib option 'testcases-dir' is required when testcases are enabled", Here());
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

    parsed.generateTestcases = config.has("generate-testcases") ? config.getBool("generate-testcases") : false;

    if (config.has("testcases-dir")) {
        const std::string testcasesDir = config.getString("testcases-dir");
        if (!testcasesDir.empty()) {
            parsed.testcasesDir = testcasesDir;
        }
    }

    parsed.apiOptions = implementation::parseMars2GribApiOptions(config);

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
        if (context.apiOptions) {
            metkit::mars2grib::Mars2Grib encoder(*context.apiOptions);
            result.encoded = encoder.encode(values, mars, misc);
        }
        else {
            metkit::mars2grib::Mars2Grib encoder;
            result.encoded = encoder.encode(values, mars, misc);
        }
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = MarsToGribCode::EncodeFailed;
        return result;
    }

    if (testCaseSink != nullptr) {
        std::string testCase;
        try {
            if (context.apiOptions) {
                metkit::mars2grib::Mars2GribTestCaseGenerator generator(*context.apiOptions);
                testCase = generator.generate(mars, misc) + "\n";
            }
            else {
                metkit::mars2grib::Mars2GribTestCaseGenerator generator;
                testCase = generator.generate(mars, misc) + "\n";
            }
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
