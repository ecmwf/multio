/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Razvan Aguridan

/// @date Jul 2023

#pragma once

#include <atomic>
#include <map>
#include <thread>
#include <vector>

#include "RingBuffer.h"

namespace multio::util {

class Tracer {
public:
    Tracer(uint32_t numChunks, uint32_t chunkSize, const std::string& outputPath);
    ~Tracer();

    void startWriterThread();

    void recordEvent(uint64_t event);

private:
    Tracer(Tracer const&) = delete;
    Tracer& operator=(Tracer const&) = delete;

    Tracer(Tracer const&&) = delete;
    Tracer& operator=(Tracer const&&) = delete;

    void writerThread_();

    const uint32_t chunkSize_;
    std::vector<uint64_t*> traceChunks_;
    std::atomic_uint32_t currentChunkAndIndex_;
    RingBuffer<uint32_t> availableQueue_;
    RingBuffer<uint32_t> writeQueue_;
    std::string outputFile_;
    std::atomic_bool running_;
    std::thread traceWriterThread_;
};

}  // namespace multio::util