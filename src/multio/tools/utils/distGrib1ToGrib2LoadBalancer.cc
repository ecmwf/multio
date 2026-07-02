/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/distGrib1ToGrib2LoadBalancer.h"

#include <algorithm>
#include <cerrno>
#include <cstring>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <queue>
#include <stdexcept>
#include <string>
#include <sys/stat.h>

namespace multio::distGrib1ToGrib2 {

long fileSizeBytes(const std::string& path) {
    struct stat st {};

    if (::stat(path.c_str(), &st) != 0) {
        throw std::runtime_error("stat failed for '" + path + "': " + std::strerror(errno));
    }

    if (!S_ISREG(st.st_mode)) {
        throw std::runtime_error("not a regular file: " + path);
    }

    if (st.st_size < 0) {
        throw std::runtime_error("negative file size reported for: " + path);
    }

    if (static_cast<unsigned long long>(st.st_size) > static_cast<unsigned long long>(std::numeric_limits<long>::max())) {
        throw std::runtime_error("file too large for long: " + path);
    }

    return static_cast<long>(st.st_size);
}

std::vector<FileWithSize> loadFileListWithSizes(const std::string& listFile) {
    std::ifstream in(listFile);
    if (!in) {
        throw std::runtime_error("cannot open input list: " + listFile);
    }

    std::vector<FileWithSize> files;
    std::string path;
    while (std::getline(in, path)) {
        if (!path.empty() && path.back() == '\r') {
            path.pop_back();
        }
        if (path.empty()) {
            continue;
        }

        files.emplace_back(path, fileSizeBytes(path));
    }

    if (!in.eof()) {
        throw std::runtime_error("error while reading input list: " + listFile);
    }

    return files;
}

SplitResult makeBalancedChunks(std::vector<FileWithSize> files, std::size_t nChunks) {
    if (nChunks == 0) {
        throw std::runtime_error("N must be > 0");
    }

    std::sort(files.begin(), files.end(), [](const FileWithSize& a, const FileWithSize& b) { return a.second > b.second; });

    SplitResult result;
    result.chunks.resize(nChunks);
    result.weights.assign(nChunks, 0);

    using HeapItem = std::pair<long long, std::size_t>;
    std::priority_queue<HeapItem, std::vector<HeapItem>, std::greater<HeapItem>> heap;
    for (std::size_t i = 0; i < nChunks; ++i) {
        heap.emplace(0, i);
    }

    for (const auto& [filename, size] : files) {
        auto [currentWeight, chunkId] = heap.top();
        (void)currentWeight;
        heap.pop();

        result.chunks[chunkId].push_back(filename);
        result.weights[chunkId] += static_cast<long long>(size);
        heap.emplace(result.weights[chunkId], chunkId);
    }

    return result;
}

void writeChunkReport(const SplitResult& result, const std::string& reportFile) {
    if (result.chunks.size() != result.weights.size()) {
        throw std::runtime_error("internal error: chunks/weights size mismatch");
    }
    if (result.weights.empty()) {
        throw std::runtime_error("cannot write report for empty split result");
    }

    long long totalWeight = 0;
    long long minWeight = result.weights[0];
    long long maxWeight = result.weights[0];
    for (long long w : result.weights) {
        totalWeight += w;
        minWeight = std::min(minWeight, w);
        maxWeight = std::max(maxWeight, w);
    }

    const double idealWeight = static_cast<double>(totalWeight) / static_cast<double>(result.weights.size());
    const long long maxMinusMin = maxWeight - minWeight;
    const double imbalancePercent = idealWeight > 0.0 ? 100.0 * static_cast<double>(maxMinusMin) / idealWeight : 0.0;
    const double maxOverIdealPercent = idealWeight > 0.0 ? 100.0 * (static_cast<double>(maxWeight) - idealWeight) / idealWeight : 0.0;

    std::ofstream out(reportFile);
    if (!out) {
        throw std::runtime_error("cannot open report file: " + reportFile);
    }

    out << "chunk_id,n_files,total_weight_bytes,delta_from_ideal_bytes,delta_from_ideal_percent\n";
    for (std::size_t i = 0; i < result.weights.size(); ++i) {
        const double delta = static_cast<double>(result.weights[i]) - idealWeight;
        const double deltaPercent = idealWeight > 0.0 ? 100.0 * delta / idealWeight : 0.0;
        out << i << ',' << result.chunks[i].size() << ',' << result.weights[i] << ',' << static_cast<long long>(delta) << ','
            << deltaPercent << '\n';
    }

    out << '\n';
    out << "summary,value\n";
    out << "total_weight_bytes," << totalWeight << '\n';
    out << "n_chunks," << result.weights.size() << '\n';
    out << "ideal_weight_per_chunk_bytes," << idealWeight << '\n';
    out << "min_chunk_weight_bytes," << minWeight << '\n';
    out << "max_chunk_weight_bytes," << maxWeight << '\n';
    out << "max_minus_min_bytes," << maxMinusMin << '\n';
    out << "imbalance_percent_max_minus_min_over_ideal," << imbalancePercent << '\n';
    out << "max_over_ideal_percent," << maxOverIdealPercent << '\n';
}

void printSplitSummaryToStderr(const SplitResult& result) {
    if (result.weights.empty()) {
        return;
    }

    long long totalWeight = 0;
    long long minWeight = result.weights[0];
    long long maxWeight = result.weights[0];
    for (long long w : result.weights) {
        totalWeight += w;
        minWeight = std::min(minWeight, w);
        maxWeight = std::max(maxWeight, w);
    }

    const double idealWeight = static_cast<double>(totalWeight) / static_cast<double>(result.weights.size());
    const double imbalancePercent = idealWeight > 0.0 ? 100.0 * static_cast<double>(maxWeight - minWeight) / idealWeight : 0.0;

    std::cerr << "chunks: " << result.chunks.size() << '\n';
    std::cerr << "total weight bytes: " << totalWeight << '\n';
    std::cerr << "ideal weight per chunk bytes: " << idealWeight << '\n';
    std::cerr << "min chunk weight bytes: " << minWeight << '\n';
    std::cerr << "max chunk weight bytes: " << maxWeight << '\n';
    std::cerr << "max-min imbalance percent: " << imbalancePercent << "%\n";
}

}  // namespace multio::distGrib1ToGrib2
