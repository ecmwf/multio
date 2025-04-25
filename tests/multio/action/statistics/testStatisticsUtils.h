
#pragma once

#include <random>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/testing/Test.h"

#include "multio/action/Plan.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/config/PathConfiguration.h"

namespace multio::test {


template <typename T>
std::vector<T> randomVector(size_t size, T min, T max, std::uint32_t seed = 42) {
    std::mt19937 gen(seed);

    std::vector<T> v(size);

    if constexpr (std::is_integral<T>::value) {
        std::uniform_int_distribution<T> dis(min, max);
        std::transform(v.begin(), v.end(), v.begin(), [&dis, &gen]() { return dis(gen); });
    }
    else if constexpr (std::is_floating_point<T>::value) {
        std::uniform_real_distribution<T> dis(min, max);
        std::transform(v.begin(), v.end(), v.begin(), [&dis, &gen](T val) { return dis(gen); });
    }

    return v;
}


multio::message::Message createStatisticsMessage(long step, std::vector<double> payload) {
    eckit::Buffer payloadBuf{payload.data(), payload.size() * sizeof(double)};
    multio::message::Metadata metadata{{{"paramId", 0},
                                        {"level", 0},
                                        {"levtype", "none"},
                                        {"gridType", "none"},
                                        {"misc-precision", "double"},
                                        {"startDate", 0},
                                        {"startTime", 0},
                                        {"step", step},
                                        {"bitmapPresent", false},
                                        {"missingValue", -999.0}}};

    return multio::message::Message{{multio::message::Message::Tag::Field, {}, {}, std::move(metadata)},
                                    std::move(payloadBuf)};
}


}  // namespace multio::test
