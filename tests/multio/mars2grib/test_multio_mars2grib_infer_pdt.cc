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

#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/generated/InferPDTTest.h"


namespace multio::test {


CASE("Test mapping pdt from metadata") {
    using namespace multio::mars2grib::rules::test;
    using namespace multio::mars2grib::rules;
    for (const PdtWithSelector& pdtWithSelector : mappedPdtAndSelectors) {
        auto pdtRec = dm::readRecord<PDTCat>(pdtWithSelector.selector);
        auto pdt = InferPdt{}.inferProductDefinitionTemplateNumber(pdtRec);
        EXPECT_EQUAL(pdt, pdtWithSelector.productDefinitionTemplateNumber);
    }
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
