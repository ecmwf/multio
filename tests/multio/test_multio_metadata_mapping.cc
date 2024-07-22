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

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/testing/Test.h"
#include "multio/action/Plan.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/config/PathConfiguration.h"
#include "multio/message/Message.h"
#include "multio/message/Metadata.h"
#include "multio/util/VariantHelpers.h"


namespace multio::test {

using multio::message::Metadata;
using multio::message::MetadataException;


CASE("Test mapping tol") {
    config::MultioConfiguration multioConf{};
    const auto& mappings = multioConf.getMetadataMappings("{~}/metadata-mapping/test-mapping-tol.yaml");

    multio::message::MetadataMappingOptions opts{};
    opts.enforceMatch = true;
    opts.overwriteExisting = true;

    EXPECT(mappings.size() == 1);


    {
        message::Metadata md{{{"paramId", 187}}};
        EXPECT_NO_THROW(mappings[0].applyInplace(md, opts));
        auto searchTOL = md.find("typeOfLevel");

        EXPECT(searchTOL != md.end());
        EXPECT_EQUAL(searchTOL->second.get<std::string>(), "mediumCloudLayer");
    }

    {
        message::Metadata md{{{"paramId", 187}, {"typeOfLevel", "overwrite me"}}};
        EXPECT_NO_THROW(mappings[0].applyInplace(md, opts));
        auto searchTOL = md.find("typeOfLevel");

        EXPECT(searchTOL != md.end());
        EXPECT_EQUAL(searchTOL->second.get<std::string>(), "mediumCloudLayer");
    }


    {
        opts.enforceMatch = true;
        message::Metadata md{{{"paramId", 333}}};
        EXPECT_THROWS_AS(mappings[0].applyInplace(md, opts), MetadataException);
    }

    {
        opts.enforceMatch = false;
        message::Metadata md{{{"paramId", 333}}};
        EXPECT_NO_THROW(mappings[0].applyInplace(md, opts));
        auto searchTOL = md.find("typeOfLevel");

        EXPECT(searchTOL == md.end());
    }


    {
        opts.enforceMatch = true;
        opts.overwriteExisting = false;
        message::Metadata md{{{"paramId", 187}, {"typeOfLevel", "still here"}}};
        EXPECT_NO_THROW(mappings[0].applyInplace(md, opts));
        auto searchTOL = md.find("typeOfLevel");

        EXPECT(searchTOL != md.end());
        EXPECT_EQUAL(searchTOL->second.get<std::string>(), "still here");
    }
};


CASE("Test mapping tol (action)") {
    std::string actions(R"json({
                  "name": "Test tol",
                  "actions" : [{
                        "type": "print",
                        "stream": "cout",
                        "prefix": "PRE MAPPING"
                    },
                    {
                      "type" : "metadata-mapping",
                      "mapping" : "{~}/metadata-mapping/test-mapping-tol.yaml",
                      "enforce-matching": false,
                      "overwrite-existing": true
                    }, 
                    {
                        "type": "print",
                        "stream": "cout",
                        "prefix": "POST MAPPING"
                    },
                    {
                      "type" : "debug-sink"
                    }]
                }
                )json");

    config::ConfigAndPaths configAndPaths;
    configAndPaths.paths = config::defaultConfigPaths();
    configAndPaths.parsedConfig = eckit::LocalConfiguration{eckit::YAMLConfiguration(actions)};

    config::MultioConfiguration multioConf{configAndPaths};
    auto& debugSink = multioConf.debugSink();

    multio::action::Plan plan{config::ComponentConfiguration{multioConf.parsedConfig(), multioConf}};

    {
        EXPECT_NO_THROW(plan.process(
            message::Message{{message::Message::Tag::Field, {}, {}, message::Metadata{{{"paramId", 187}}}}}));
        EXPECT(debugSink.size() == 1);
        const auto& md = debugSink.front().metadata();
        auto searchTOL = md.find("typeOfLevel");

        EXPECT(searchTOL != md.end());
        EXPECT_EQUAL(searchTOL->second.get<std::string>(), "mediumCloudLayer");
    }
};


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
