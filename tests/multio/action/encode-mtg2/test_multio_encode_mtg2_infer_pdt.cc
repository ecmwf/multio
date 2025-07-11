/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/testing/Test.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/action/encode-mtg2/EncoderConf.h"
#include "multio/action/encode-mtg2/Options.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"
#include "multio/util/Substitution.h"

#include "multio/action/encode-mtg2/generated/InferPDT.h"
#include "multio/action/encode-mtg2/generated/InferPDTTest.h"

#include <unordered_set>


namespace multio::test {


CASE("Test mapping pdt from metadata") {
    using namespace multio::action::rules::test;
    using namespace multio::action::rules;
    for(const PdtWithSelector& pdtWithSelector: mappedPdtAndSelectors) {
        auto pdt = InferPdt{}.inferProductDefinitionTemplateNumber(read(PDTCatKeySet{}, pdtWithSelector.selector));
        EXPECT_EQUAL(pdt, pdtWithSelector.productDefinitionTemplateNumber);
    }
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
