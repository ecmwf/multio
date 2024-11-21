/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// @author Razvan Aguridan

#include <chrono>
#include <thread>

#include "eckit/testing/Test.h"

#include "multio/util/RingBuffer.h"

namespace multio::test {

CASE("Test SP/SC.") {
    multio::util::RingBuffer<uint32_t> rb(50);

    auto t = std::thread([&rb]() {
        for (uint32_t i = 0; i < 100; ++i) {
            rb.push(i);
        }
    });

    uint32_t sum = 0;
    for (uint32_t i = 0; i < 100; ++i) {
        auto item = rb.pop();
        while (!item) {
            item = rb.pop();
        }

        sum += item.value();
    }

    t.join();

    EXPECT(sum == 4950);
}

CASE("Test MP/SC.") {
    multio::util::RingBuffer<uint32_t> rb(100);

    std::vector<std::thread> threads;
    for (uint32_t i = 0; i < 10; ++i) {
        threads.push_back(std::thread([&rb, i]() {
            for (uint32_t j = 0; j < 10; ++j) {
                rb.push(i * 10 + j);
            }
        }));
    }

    uint32_t sum = 0;
    for (uint32_t i = 0; i < 100; ++i) {
        auto item = rb.pop();
        while (!item) {
            item = rb.pop();
        }

        sum += item.value();
    }

    for (auto& t : threads) {
        t.join();
    }

    EXPECT(sum == 4950);
}

CASE("Test SP/MC.") {
    multio::util::RingBuffer<uint32_t> rb(50);

    std::vector<std::thread> threads;
    for (uint32_t i = 0; i < 10; ++i) {
        threads.push_back(std::thread([&rb]() {
            for (uint32_t j = 0; j < 10; ++j) {
                auto item = rb.pop();
                while (!item) {
                    item = rb.pop();
                }

                EXPECT(item.value() >= 0);
                EXPECT(item.value() <= 99);
            }
        }));
    }

    for (uint32_t i = 0; i < 100; ++i) {
        rb.push(i);
    }

    for (auto& t : threads) {
        t.join();
    }
}

CASE("Test MP/MC.") {
    multio::util::RingBuffer<uint32_t> rb(100);

    std::vector<std::thread> consumers;
    for (uint32_t i = 0; i < 10; ++i) {
        consumers.push_back(std::thread([&rb]() {
            for (uint32_t j = 0; j < 10; ++j) {
                auto item = rb.pop();
                while (!item) {
                    item = rb.pop();
                }

                EXPECT(item.value() >= 0);
                EXPECT(item.value() <= 99);
            }
        }));
    }

    std::vector<std::thread> producers;
    for (uint32_t i = 0; i < 10; ++i) {
        producers.push_back(std::thread([&rb, i]() {
            for (uint32_t j = 0; j < 10; ++j) {
                rb.push(i * 10 + j);
            }
        }));
    }

    for (auto& t : producers) {
        t.join();
    }

    for (auto& t : consumers) {
        t.join();
    }
}

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}