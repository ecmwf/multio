
# pragma once

#include <random>

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
}


class MultioTestEnvironment {
public:
    MultioTestEnvironment() = delete;

    MultioTestEnvironment(config::MultioConfiguration multioConfig) :
        multioConfig_{std::move(multioConfig)},
        plan_{config::ComponentConfiguration(multioConfig_.parsedConfig(), multioConfig_)} {}

    MultioTestEnvironment(const std::string& actions) : MultioTestEnvironment(configFromActions(actions)) {}

    config::MultioConfiguration& multioConfig() {
        return multioConfig_;
    }

    multio::action::Plan& plan() {
        return plan_;
    }

    std::queue<message::Message>& debugSink() {
        return multioConfig_.debugSink();
    }

private:
    config::MultioConfiguration multioConfig_;
    multio::action::Plan plan_;
};


template<typename T>
std::vector<T> randomVector(size_t size, T min, T max, std::uint32_t seed = 42) {
    std::mt19937 gen(seed);

    std::vector<T> v(size);

    if constexpr(std::is_integral<T>::value) {
        std::uniform_int_distribution<T> dis(min, max);
        std::transform(v.begin(), v.end(), v.begin(), [&dis, &gen]() { return dis(gen); });
    }
    else if constexpr(std::is_floating_point<T>::value) {
        std::uniform_real_distribution<T> dis(min, max);
        std::transform(v.begin(), v.end(), v.begin(), [&dis, &gen](T val) { return dis(gen); });
    }

    return v;
}


multio::message::Message createStatisticsMessage(long step, std::vector<double> payload) {
    eckit::Buffer payloadBuf{payload.data(), payload.size() * sizeof(double)};
    multio::message::Metadata metadata{{
        { "paramId", 0 },
        { "level", 0 },
        { "levtype", "none" },
        { "gridType", "none" },
        { "precision", "double" },
        { "startDate", 0 },
        { "startTime", 0 },
        { "step", step },
        { "bitmapPresent", false },
        { "missingValue", -999.0 }
    }};

    return multio::message::Message{
        {multio::message::Message::Tag::Field, {}, {}, std::move(metadata)},
        std::move(payloadBuf)
    };
}


}  // namespace multio::test
