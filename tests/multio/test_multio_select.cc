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


CASE("Test enfore same key type") {
    {
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

        EXPECT_THROWS_AS(sel.matches(message::Metadata{{{"name", 1}}}), message::MetadataException);
    }
    {
        std::stringstream confString;
        confString << R"json(
              {
                  "enforce-same-key-types": false,
                  "match": {
                      "name": [ "a", "b" ]
                  },
                  "ignore": {
                      "format": "grib"
                  }
            })json";

        message::match::MatchReduce sel{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
        EXPECT(!sel.isEmpty());

        EXPECT_EQUAL(sel.matches(message::Metadata{{{"name", 1}}}), false);
    }
}

//-----------------------------------------------------------------------------
// Numeric conditions

namespace {
message::match::MatchReduce makeSelector(const std::string& yaml) {
    std::stringstream confString;
    confString << yaml;
    return message::match::MatchReduce{eckit::LocalConfiguration{eckit::YAMLConfiguration(confString)}};
}

message::Metadata md(const std::string& key, std::int64_t v) {
    return message::Metadata{{{key, v}}};
}
message::Metadata md(const std::string& key, double v) {
    return message::Metadata{{{key, v}}};
}
}  // namespace

CASE("Numeric condition - from/by/to") {
    auto sel = makeSelector(R"yaml(
match:
  step:
    from: 0
    by: 5
    to: 100
)yaml");
    std::cout << sel << std::endl;

    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{0})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{5})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{100})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{3})), false);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{105})), false);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{-5})), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"level", 1}}}), false);  // missing key
}

CASE("Numeric condition - from/by without to") {
    auto sel = makeSelector(R"yaml(
match:
  step:
    from: 6
    by: 6
)yaml");

    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{0})), false);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{6})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{600})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{601})), false);
}

CASE("Numeric condition - from/to without by is an interval") {
    auto sel = makeSelector(R"yaml(
match:
  step:
    from: 10
    to: 20
)yaml");

    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{9})), false);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{10})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{17})), true);
    EXPECT_EQUAL(sel.matches(md("step", 15.5)), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{20})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{21})), false);
}

CASE("Numeric condition - vectors of segments") {
    auto sel = makeSelector(R"yaml(
match:
  step:
    from: [1, 100, 500]
    to: [100, 500, 1000]
    by: [1, 5, 10]
)yaml");

    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{0})), false);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{1})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{99})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{100})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{103})), false);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{105})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{505})), false);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{510})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{1000})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{1010})), false);
}

CASE("Numeric condition - scalars are broadcast") {
    auto sel = makeSelector(R"yaml(
match:
  step:
    from: [0, 240]
    to: [120, 360]
    by: 6
)yaml");

    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{114})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{126})), false);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{246})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{247})), false);
}

CASE("Numeric condition - bounds") {
    auto gt = makeSelector("match:\n  step:\n    greater-than: 10\n");
    EXPECT_EQUAL(gt.matches(md("step", std::int64_t{10})), false);
    EXPECT_EQUAL(gt.matches(md("step", std::int64_t{11})), true);

    auto ge = makeSelector("match:\n  step:\n    greater-equal-to: 10\n");
    EXPECT_EQUAL(ge.matches(md("step", std::int64_t{9})), false);
    EXPECT_EQUAL(ge.matches(md("step", std::int64_t{10})), true);

    auto lt = makeSelector("match:\n  step:\n    lower-than: 10\n");
    EXPECT_EQUAL(lt.matches(md("step", std::int64_t{10})), false);
    EXPECT_EQUAL(lt.matches(md("step", std::int64_t{9})), true);

    auto le = makeSelector("match:\n  step:\n    lower-equal-to: 10\n");
    EXPECT_EQUAL(le.matches(md("step", std::int64_t{11})), false);
    EXPECT_EQUAL(le.matches(md("step", std::int64_t{10})), true);

    // Bounds are combined with AND, also with a from/by progression
    auto both = makeSelector(R"yaml(
match:
  step:
    from: 0
    by: 3
    greater-than: 6
    lower-equal-to: 12
)yaml");
    EXPECT_EQUAL(both.matches(md("step", std::int64_t{6})), false);
    EXPECT_EQUAL(both.matches(md("step", std::int64_t{9})), true);
    EXPECT_EQUAL(both.matches(md("step", std::int64_t{10})), false);
    EXPECT_EQUAL(both.matches(md("step", std::int64_t{12})), true);
    EXPECT_EQUAL(both.matches(md("step", std::int64_t{15})), false);
}

CASE("Numeric condition - floating point") {
    auto sel = makeSelector(R"yaml(
match:
  time:
    from: 0.0
    by: 0.1
    to: 1.0
)yaml");

    EXPECT_EQUAL(sel.matches(md("time", 0.3)), true);
    EXPECT_EQUAL(sel.matches(md("time", 0.35)), false);
    EXPECT_EQUAL(sel.matches(md("time", std::int64_t{1})), true);
}

CASE("Numeric condition - list of conditions is an OR, other keys still AND") {
    auto sel = makeSelector(R"yaml(
match:
  param: [167, 168]
  step:
    - lower-than: 3
    - from: 24
      by: 24
)yaml");

    EXPECT_EQUAL(sel.matches(message::Metadata{{{"param", 167}, {"step", 0}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"param", 167}, {"step", 48}}}), true);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"param", 167}, {"step", 12}}}), false);
    EXPECT_EQUAL(sel.matches(message::Metadata{{{"param", 130}, {"step", 48}}}), false);
}

CASE("Numeric condition - works with ignore and not") {
    auto sel = makeSelector(R"yaml(
ignore:
  step:
    greater-than: 240
)yaml");
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{240})), true);
    EXPECT_EQUAL(sel.matches(md("step", std::int64_t{246})), false);
}

CASE("Numeric condition - non numeric metadata") {
    auto strict = makeSelector("match:\n  step:\n    from: 0\n");
    EXPECT_THROWS_AS(strict.matches(message::Metadata{{{"step", "0"}}}), message::MetadataException);

    auto lax = makeSelector("enforce-same-key-types: false\nmatch:\n  step:\n    from: 0\n");
    EXPECT_EQUAL(lax.matches(message::Metadata{{{"step", "0"}}}), false);
}

CASE("Numeric condition - configuration errors") {
    EXPECT_THROWS_AS(makeSelector("match:\n  step:\n    form: 0\n"), message::MetadataException);  // typo
    EXPECT_THROWS_AS(makeSelector("match:\n  step:\n    to: 10\n"), message::MetadataException);   // no from
    EXPECT_THROWS_AS(makeSelector("match:\n  step:\n    from: 10\n    to: 0\n"), message::MetadataException);
    EXPECT_THROWS_AS(makeSelector("match:\n  step:\n    from: 0\n    by: 0\n"), message::MetadataException);
    EXPECT_THROWS_AS(makeSelector("match:\n  step:\n    from: [0, 1]\n    to: [1, 2, 3]\n"),
                     message::MetadataException);
    EXPECT_THROWS_AS(makeSelector("match:\n  step:\n    lower-than: [1, 2]\n"), message::MetadataException);
    EXPECT_THROWS_AS(makeSelector("match:\n  step: {}\n"), message::MetadataException);
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
