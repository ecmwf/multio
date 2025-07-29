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
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/EncoderConf.h"
#include "multio/mars2grib/Rules.h"
#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/rules/Rule.h"
#include "multio/mars2grib/sections/Level.h"
#include "multio/mars2grib/sections/SectionSetter.h"

#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/message/Metadata.h"

#include "multio/util/MioGribHandle.h"
#include "test_multio_mars2grib_helper.h"

#include <iostream>


namespace multio::test {

CASE("Test level setter") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;
    using namespace multio::mars2grib::sections;

    std::vector<std::pair<std::string, TypeOfLevel>> tols{
        {"ml", TypeOfLevel::Hybrid},
        {"ml", TypeOfLevel::Snow},
        {"sfc", TypeOfLevel::HeightAboveGround},
        {"sfc", TypeOfLevel::HeightAboveGroundAt10m},
    };

    for (const auto& p : tols) {
        SectionCollector col;
        auto levtypeStr = p.first;
        auto tol = p.second;

        LevelKeyValueSet conf;
        key<LevelDef::Type>(conf).set(tol);
        col.add(std::make_unique<LevelSetter>(conf));

        auto md = mkMd();
        md.set("levtype", levtypeStr);
        md.set("misc-pv", std::vector<double>{{0.1, 0.2, 0.3}});

        auto mars = read(MarsKeySet{}, md);
        auto misc = read(MiscKeySet{}.makeScoped(), md);

        // auto geo = read(getGeometryKeySet(mars), md);
        // Ignore geo for now.. may cause other problems
        Geometry geo{};  // UNCHECKED

        std::cout << "Example for required level keys: " << std::endl;
        col.writeKeyInfo(std::cout, mars);

        // TBD
        // auto sample = util::MioGribHandle::makeDefault();
        // col.prepare(*sample.get(), mars, misc, geo);
        // col.allocate(*sample.get(), mars, misc, geo);
        // col.preset(*sample.get(), mars, misc, geo);
        // col.runtime(*sample.get(), mars, misc, geo);
        // col.check(*sample.get(), mars, misc, geo);
    }
}


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
