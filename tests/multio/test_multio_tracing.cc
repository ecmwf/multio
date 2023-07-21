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

#include <atomic>
#include <chrono>
#include <iostream>
#include <thread>

#include "eckit/testing/Test.h"

#include "multio/util/Tracer.h"

namespace multio::test {
CASE("Test single thread tracing.") {
    multio::util::Tracer tracer(8, 32768, "./trace_info.bin");

    tracer.startWriterThread();

    const auto startTime = std::chrono::high_resolution_clock::now();

    for (auto i = 1; i <= 1000000; ++i) {
        tracer.recordEvent(i);
    }

    const auto endTime = std::chrono::high_resolution_clock::now();
    std::cout << "Time to trace 1000000 events from 1 single thread: " << (endTime - startTime).count() << " ns!"
              << std::endl;
}

CASE("Test multiple thread tracing.") {
    multio::util::Tracer tracer(8, 32768, "./trace_info2.bin");

    tracer.startWriterThread();

    std::atomic_bool start = false;

    std::vector<std::thread> producers;
    for (uint32_t i = 0; i < 10; ++i) {
        producers.push_back(std::thread([&tracer, &start, i]() {
            while (!start.load(std::memory_order_acquire))
                ;

            for (auto j = 1; j < 1000001; ++j) {
                tracer.recordEvent(i * 10 + j);
            }
        }));
    }

    const auto startTime = std::chrono::high_resolution_clock::now();

    start.store(true, std::memory_order_release);

    for (auto& t : producers) {
        t.join();
    }

    const auto endTime = std::chrono::high_resolution_clock::now();
    std::cout << "Time to trace 10000000 events from 10 threads: " << (endTime - startTime).count() << " ns!"
              << std::endl;
}
}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}