/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <fstream>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/JSON.h"
#include "eckit/testing/Test.h"

#include "multio/message/Metadata.h"
#include "multio/message/MetadataSelector.h"

#include "TestDataContent.h"
#include "TestHelpers.h"

namespace multio {
namespace test {

//-----------------------------------------------------------------------------

CASE("Test basic empty selector") {
    std::stringstream confString;
    confString << R"json(
          {
            "selectors" : []
        })json";

    message::MetadataSelectors sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(sel.isEmpty());
    EXPECT_EQUAL(sel.matches(message::Metadata{}), false);
}


CASE("Test basic selector with 1 field") {
    std::stringstream confString;
    confString << R"json(
          {
            "selectors" : [
                  {
                      "match": [{
                          "name": [ "a", "b", "c"]
                      }]
                  }
                ]
        })json";

    message::MetadataSelectors sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(!sel.isEmpty());

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 10}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 20}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 10}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 20}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 10}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 20}}}), true);

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 1}}}), false);
}

CASE("Test basic selector with 2 fields") {
    std::stringstream confString;
    confString << R"json(
          {
            "selectors" : [
                  {
                      "match": [{
                          "name": [ "a", "b", "c"],
                          "level": [ 1, 10, 20]
                      }]
                  }
                ]
        })json";

    message::MetadataSelectors sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(!sel.isEmpty());

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 10}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 20}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 10}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 20}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 10}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 20}}}), true);

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 1}}}), false);
}

CASE("Test basic selector with multiple match in 1 selector") {
    std::stringstream confString;
    confString << R"json(
          {
            "selectors" : [
                  {
                      "match": [{
                          "name": [ "a", "b"],
                          "level": [ 1, 2]
                      },
                      {
                          "name": [ "c", "d"],
                          "level": [ 3, 4]
                      }]
                  }
                ]
        })json";

    message::MetadataSelectors sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(!sel.isEmpty());

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 4}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 4}}}), true);

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 3}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 4}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 3}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 4}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 1}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 1}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 2}}}), false);
}


CASE("Test basic selector with multiple match in 2 selectors") {
    std::stringstream confString;
    confString << R"json(
          {
            "selectors" : [
                  {
                      "match": [{
                          "name": [ "a", "b"],
                          "level": [ 1, 2]
                      }]
                  },
                  {
                      "match": [{
                          "name": [ "c", "d"],
                          "level": [ 3, 4]
                      }]
                  }
                ]
        })json";

    message::MetadataSelectors sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(!sel.isEmpty());

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 4}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 4}}}), true);

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 3}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 4}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 3}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 4}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 1}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 1}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 2}}}), false);
}


CASE("Test basic selector with ignore") {
    std::stringstream confString;
    confString << R"json(
          {
            "selectors" : [
                  {
                      "ignore": [{
                          "name": [ "a", "b" ],
                          "level": [ 1, 2]
                      }]
                  }
                ]
        })json";

    message::MetadataSelectors sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(!sel.isEmpty());

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 1}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 1}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 4}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 4}}}), true);

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 4}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 4}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 2}}}), true);
}


CASE("Test basic selector with mutlile ignore in 1 selector") {
    std::stringstream confString;
    confString << R"json(
          {
            "selectors" : [
                  {
                      "ignore": [{
                          "name": [ "a", "b" ],
                          "level": [ 1, 2]
                      }, {
                          "name": [ "c", "d" ],
                          "level": [ 3, 4]
                      }]
                  }
                ]
        })json";

    message::MetadataSelectors sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(!sel.isEmpty());

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 1}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 1}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 2}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 3}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 4}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 3}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 4}}}), false);

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"level", 4}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 3}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"level", 4}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"level", 2}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 1}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "d"}, {"level", 2}}}), true);
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
