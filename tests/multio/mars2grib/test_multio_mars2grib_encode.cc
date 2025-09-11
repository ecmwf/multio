/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/testing/Test.h"

#include "multio/datamod/AtlasGeo.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Encoder.h"

#include "multio/datamod/AtlasGeo.h"

#include "multio/message/Metadata.h"

#include "multio/util/SampleMetadataGen.h"


namespace multio::test {

namespace dm = multio::datamod;

CASE("Test encoder.getHandle with AIFS single keys") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;

    Encoder encoder{};
    dm::MiscRecord misc;
    dm::applyRecordDefaults(misc);
    dm::validateRecord(misc);

    for (auto md : multio::util::sample_gen::mkAifsSingleMd()) {
        try {
            auto mars = dm::readRecord<dm::FullMarsRecord>(md);
            auto geo = dm::makeUnscopedGeometry(mars);
            auto handle = encoder.getHandle(mars, misc, geo);
        }
        catch (...) {
            std::cout << "Error while generating & comparing message: " << md << std::endl;
            throw;
        }
    }
};

CASE("Test encoder.getHandle with AIFS ens keys") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;

    Encoder encoder{};
    dm::MiscRecord misc;
    misc.typeOfEnsembleForecast.set(1);
    misc.numberOfForecastsInEnsemble.set(50);
    dm::applyRecordDefaults(misc);
    dm::validateRecord(misc);

    for (auto md : multio::util::sample_gen::mkAifsEnsMd()) {
        try {
            auto mars = dm::readRecord<dm::FullMarsRecord>(md);
            auto geo = dm::makeUnscopedGeometry(mars);
            auto handle = encoder.getHandle(mars, misc, geo);
        }
        catch (...) {
            std::cout << "Error while generating & comparing message: " << md << std::endl;
            throw;
        }
    }
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
