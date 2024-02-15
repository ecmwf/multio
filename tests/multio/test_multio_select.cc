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
#include "multio/message/MetadataMatcher.h"

#include "TestDataContent.h"
#include "TestHelpers.h"

namespace multio {
namespace test {

//-----------------------------------------------------------------------------

CASE("Test basic empty selector") {
    {
        std::stringstream confString;
        confString << R"json(
              {
                "any" : []
            })json";

        message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
        EXPECT(sel.isEmpty());
        EXPECT_EQUAL(sel.matches(message::Metadata{}), false);
    }
    {
        std::stringstream confString;
        confString << R"json(
              {
                "all" : []
            })json";

        message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
        EXPECT(sel.isEmpty());
        EXPECT_EQUAL(sel.matches(message::Metadata{}), true);
    }
}


CASE("Test basic selector with 1 field - any match") {
    {
        std::stringstream confString;
        confString << R"json(
              {
                "any" : [
                      {
                          "match": [{
                              "name": [ "a", "b", "c"]
                          }]
                      }
                    ]
            })json";

        message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
        EXPECT(!sel.isEmpty());

        std::cout << sel;

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
}

CASE("Test basic selector with 1 field - match") {
    {
        std::stringstream confString;
        confString << R"json(
              {
                  "match": [{
                      "name": [ "a", "b", "c"]
                  }]
            })json";

        message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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
}

CASE("Test basic selector with 2 fields - any match") {
    {
        std::stringstream confString;
        confString << R"json(
              {
                "any" : [
                      {
                          "match": [{
                              "name": [ "a", "b", "c"],
                              "level": [ 1, 10, 20]
                          }]
                      }
                    ]
            })json";

        message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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
}

CASE("Test basic selector with 2 fields - match") {
    {
        std::stringstream confString;
        confString << R"json(
              {
                  "match": [{
                      "name": [ "a", "b", "c"],
                      "level": [ 1, 10, 20]
                  }]
            })json";

        message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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
}

CASE("Test basic selector with multiple match in 1 selector") {
    std::stringstream confString;
    confString << R"json(
          {
            "any" : [
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

    message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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
            "any" : [
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

    message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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
            "any" : [
                  {
                      "ignore": [{
                          "name": [ "a", "b" ],
                          "level": [ 1, 2]
                      }]
                  }
                ]
        })json";

    message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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


CASE("Test basic selector with not match (ignore)") {
    std::stringstream confString;
    confString << R"json(
          {
            "any" : [
                  {
                      "not": {
                          "match": [{
                              "name": [ "a", "b" ],
                              "level": [ 1, 2]
                          }]
                        }
                  }
                ]
        })json";

    message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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

CASE("Test basic selector with not not ignore (ignore)") {
    std::stringstream confString;
    confString << R"json(
          {
            "any" : [
                  {
                      "not": {
                        "not": {
                          "ignore": [{
                              "name": [ "a", "b" ],
                              "level": [ 1, 2]
                          }]
                        }
                      }
                  }
                ]
        })json";

    message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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
            "any" : [
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

    message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
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

CASE("Test  match + ignore combination with reduce-and") {
    std::stringstream confString;
    confString << R"json(
          {
            "all" : [
                  {
                      "match": {
                          "name": [ "a", "b" ]
                      } 
                  },
                  {
                      "ignore": {
                          "format": "grib"
                      }
                  }
                ]
        })json";

    message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(!sel.isEmpty());

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"format", "raw"}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"format", "raw"}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"format", "grib"}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"format", "grib"}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"format", "raw"}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"format", "grib"}}}), false);
}


CASE("Test default match + ignore combination with reduce-and") {
    std::stringstream confString;
    confString << R"json(
          {
              "match": {
                  "name": [ "a", "b" ]
              },
              "ignore": {
                  "format": "grib"
              }
        })json";

    message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
    EXPECT(!sel.isEmpty());

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"format", "raw"}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"format", "raw"}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "a"}, {"format", "grib"}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "b"}, {"format", "grib"}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"format", "raw"}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", "c"}, {"format", "grib"}}}), false);
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
