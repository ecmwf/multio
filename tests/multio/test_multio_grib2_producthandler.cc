/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/// @author Philipp Geier

#include <unistd.h>
#include <cstring>
#include <iostream>
#include <limits>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/testing/Test.h"

#include "multio/grib2/GeneratedProductHandler.h"
#include "multio/grib2/GeneratedProductHandlerTest.h"
#include "multio/message/Metadata.h"


using multio::message::Metadata;

namespace multio::test {

CASE("Test all product category selection") {

    grib2::Grib2ProductHandler<> grib2ProductHandler;
    for (const auto& pdtAndSelector : multio::grib2::test::mappedPdtAndSelectors) {
        std::cout << "inferProductDefinitionTemplateNumber(" << pdtAndSelector.selector << ") expected to be equal to "
                  << pdtAndSelector.productDefinitionTemplateNumber << std::endl;

        std::int64_t pdt;
        EXPECT_NO_THROW(pdt = grib2ProductHandler.inferProductDefinitionTemplateNumber(pdtAndSelector.selector));
        EXPECT_EQUAL(pdt, pdtAndSelector.productDefinitionTemplateNumber);


        EXPECT_NO_THROW(grib2ProductHandler.keysForPdt(pdt));
        for (const auto& key : grib2ProductHandler.keysForPdt(pdt).get()) {
            EXPECT_NO_THROW(grib2ProductHandler.keyInfoForKey(key));
            const auto& keyInfo = grib2ProductHandler.keyInfoForKey(key).get();


            EXPECT(keyInfo.types.size() > 0);
        }
    }
}


CASE("Test invalid product category selections") {

    grib2::Grib2ProductHandler<> grib2ProductHandler;
    EXPECT_THROWS(grib2ProductHandler.inferProductDefinitionTemplateNumber({}));
}

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
