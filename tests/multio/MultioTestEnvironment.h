
#pragma once

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/testing/Test.h"

#include "multio/action/Plan.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/config/PathConfiguration.h"


namespace multio::test {


namespace {
config::ConfigAndPaths configFromActions(const std::string& actions) {
    config::ConfigAndPaths configAndPaths;
    configAndPaths.paths = config::defaultConfigPaths();
    configAndPaths.parsedConfig = eckit::LocalConfiguration{eckit::YAMLConfiguration(actions)};
    return configAndPaths;
}
}  // namespace


class MultioTestEnvironment {
public:
    MultioTestEnvironment() = delete;

    MultioTestEnvironment(config::MultioConfiguration multioConfig) :
        multioConfig_{std::move(multioConfig)},
        plan_{config::ComponentConfiguration(multioConfig_.parsedConfig(), multioConfig_)} {}

    MultioTestEnvironment(const std::string& actions) : MultioTestEnvironment(configFromActions(actions)) {}

    config::MultioConfiguration& multioConfig() { return multioConfig_; }

    multio::action::Plan& plan() { return plan_; }

    std::queue<message::Message>& debugSink() { return multioConfig_.debugSink(); }

private:
    config::MultioConfiguration multioConfig_;
    multio::action::Plan plan_;
};

}  // namespace multio::test
