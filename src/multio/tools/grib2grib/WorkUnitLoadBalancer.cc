/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief MPI-free bucket creation utilities for `grib2grib` work units.

#include "multio/tools/grib2grib/WorkUnitLoadBalancer.h"

#include <algorithm>
#include <fstream>
#include <functional>
#include <limits>
#include <queue>
#include <stdexcept>
#include <utility>

namespace multio::distGrib1ToGrib2::grib2grib {

namespace implementation {

struct FileWithSize {
    std::string filename;
    std::uint64_t totalSizeBytes = 0;
};

struct LoadBalancePlan {
    std::vector<FileWithSize> files;
    std::vector<WorkUnit> workUnits;
    std::vector<WorkBucket> buckets;
    std::uint64_t totalSizeBytes = 0;
    std::uint64_t sizePerWorkerBytes = 0;
    std::uint64_t referenceWorkUnitSizeBytes = 0;
    std::size_t nWorkers = 0;
    std::size_t averageWorkUnitsPerWorker = 0;
};

std::uint64_t checkedAdd(std::uint64_t lhs, std::uint64_t rhs, const char* description) {
    if (rhs > std::numeric_limits<std::uint64_t>::max() - lhs) {
        throw std::overflow_error(std::string{"uint64 overflow while computing "} + description);
    }
    return lhs + rhs;
}

std::uint64_t trimWorkUnitSize(const WorkUnit& workUnit) {
    return static_cast<std::uint64_t>(workUnit.endOffset - workUnit.startOffset);
}

void appendU64(std::vector<char>& buffer, std::uint64_t value) {
    for (unsigned shift = 0; shift < 64; shift += 8) {
        buffer.push_back(static_cast<char>((value >> shift) & 0xffu));
    }
}

std::uint64_t readU64(const std::vector<char>& buffer, std::size_t& cursor) {
    if (buffer.size() - cursor < 8) {
        throw std::runtime_error("Truncated payload while reading uint64");
    }

    std::uint64_t value = 0;
    for (unsigned shift = 0; shift < 64; shift += 8) {
        value |= static_cast<std::uint64_t>(static_cast<unsigned char>(buffer[cursor++])) << shift;
    }
    return value;
}

std::string trimLine(std::string line) {
    if (!line.empty() && line.back() == '\r') {
        line.pop_back();
    }
    return line;
}

}  // namespace implementation

namespace implementation {

std::vector<FileWithSize> loadFileListWithSizes(const std::string& listFile) {
    std::ifstream in(listFile);
    if (!in) {
        throw std::runtime_error("cannot open input list: " + listFile);
    }

    std::vector<FileWithSize> files;
    std::string path;
    while (std::getline(in, path)) {
        path = trimLine(std::move(path));
        if (path.empty()) {
            continue;
        }

        files.push_back(FileWithSize{path, static_cast<std::uint64_t>(fileSizeBytes(path))});
    }

    if (!in.eof()) {
        throw std::runtime_error("error while reading input list: " + listFile);
    }

    return files;
}

std::vector<WorkUnit> makeEstimatedWorkUnits(const std::vector<FileWithSize>& files,
                                             std::uint64_t referenceWorkUnitSizeBytes) {
    if (referenceWorkUnitSizeBytes == 0) {
        throw std::invalid_argument("referenceWorkUnitSizeBytes must be > 0");
    }

    std::vector<WorkUnit> workUnits;
    for (const auto& file : files) {
        if (file.totalSizeBytes == 0) {
            continue;
        }

        if (file.totalSizeBytes <= referenceWorkUnitSizeBytes) {
            workUnits.push_back(WorkUnit{file.filename, 0, static_cast<off_t>(file.totalSizeBytes)});
            continue;
        }

        const auto splitUnits = splitFileByMaximumWorkUnitSize(file.filename, referenceWorkUnitSizeBytes);
        workUnits.insert(workUnits.end(), splitUnits.begin(), splitUnits.end());
    }

    return workUnits;
}

std::vector<WorkBucket> makeBalancedWorkBuckets(std::vector<WorkUnit> workUnits, std::size_t nWorkers) {
    if (nWorkers == 0) {
        throw std::invalid_argument("nWorkers must be > 0");
    }

    std::sort(workUnits.begin(), workUnits.end(), [](const WorkUnit& lhs, const WorkUnit& rhs) {
        const auto lhsSize = trimWorkUnitSize(lhs);
        const auto rhsSize = trimWorkUnitSize(rhs);
        if (lhsSize != rhsSize) {
            return lhsSize > rhsSize;
        }
        if (lhs.filename != rhs.filename) {
            return lhs.filename < rhs.filename;
        }
        return lhs.startOffset < rhs.startOffset;
    });

    std::vector<WorkBucket> buckets(nWorkers);

    using HeapItem = std::pair<std::uint64_t, std::size_t>;
    std::priority_queue<HeapItem, std::vector<HeapItem>, std::greater<HeapItem>> heap;
    for (std::size_t i = 0; i < nWorkers; ++i) {
        heap.emplace(0, i);
    }

    for (auto& workUnit : workUnits) {
        const auto [currentWeight, bucketId] = heap.top();
        (void)currentWeight;
        heap.pop();

        const auto weight = trimWorkUnitSize(workUnit);
        buckets[bucketId].workUnits.push_back(std::move(workUnit));
        buckets[bucketId].totalWeightBytes = checkedAdd(buckets[bucketId].totalWeightBytes, weight, "bucket weight");
        heap.emplace(buckets[bucketId].totalWeightBytes, bucketId);
    }

    return buckets;
}

LoadBalancePlan makeLoadBalancePlan(const std::string& listFile, std::size_t nWorkers,
                                    std::size_t averageWorkUnitsPerWorker) {
    if (nWorkers == 0) {
        throw std::invalid_argument("nWorkers must be > 0");
    }
    if (averageWorkUnitsPerWorker == 0) {
        throw std::invalid_argument("averageWorkUnitsPerWorker must be > 0");
    }

    LoadBalancePlan plan;
    plan.files = loadFileListWithSizes(listFile);
    plan.nWorkers = nWorkers;
    plan.averageWorkUnitsPerWorker = averageWorkUnitsPerWorker;

    for (const auto& file : plan.files) {
        plan.totalSizeBytes = checkedAdd(plan.totalSizeBytes, file.totalSizeBytes, "total input size");
    }

    plan.sizePerWorkerBytes = plan.totalSizeBytes / static_cast<std::uint64_t>(nWorkers);
    plan.referenceWorkUnitSizeBytes = std::max<std::uint64_t>(
        1, plan.sizePerWorkerBytes / static_cast<std::uint64_t>(averageWorkUnitsPerWorker));

    plan.workUnits = makeEstimatedWorkUnits(plan.files, plan.referenceWorkUnitSizeBytes);
    plan.buckets = makeBalancedWorkBuckets(plan.workUnits, nWorkers);

    return plan;
}

std::vector<char> serializeWorkBucketImpl(const WorkBucket& bucket) {
    std::vector<char> payload;
    appendU64(payload, bucket.totalWeightBytes);
    const auto serializedWorkUnits = serializeWorkUnits(bucket.workUnits);
    appendU64(payload, static_cast<std::uint64_t>(serializedWorkUnits.size()));
    payload.insert(payload.end(), serializedWorkUnits.begin(), serializedWorkUnits.end());
    return payload;
}

WorkBucket deserializeWorkBucketImpl(const std::vector<char>& payload) {
    std::size_t cursor = 0;

    WorkBucket bucket;
    bucket.totalWeightBytes = readU64(payload, cursor);
    const auto workUnitsPayloadSize = readU64(payload, cursor);
    if (workUnitsPayloadSize > static_cast<std::uint64_t>(payload.size() - cursor)) {
        throw std::runtime_error("Truncated payload while deserializing WorkBucket");
    }

    std::vector<char> workUnitsPayload(payload.begin() + static_cast<std::ptrdiff_t>(cursor),
                                       payload.begin() + static_cast<std::ptrdiff_t>(cursor + workUnitsPayloadSize));
    cursor += static_cast<std::size_t>(workUnitsPayloadSize);
    bucket.workUnits = deserializeWorkUnits(workUnitsPayload);

    if (cursor != payload.size()) {
        throw std::runtime_error("Unexpected trailing data while deserializing WorkBucket");
    }

    return bucket;
}

}  // namespace implementation



std::vector<WorkBucket> createBuckets(const std::vector<std::string>& filenames, std::size_t nWorkers,
                                      std::size_t averageWorkUnitsPerWorker) {
    if (nWorkers == 0) {
        throw std::invalid_argument("nWorkers must be > 0");
    }
    if (averageWorkUnitsPerWorker == 0) {
        throw std::invalid_argument("averageWorkUnitsPerWorker must be > 0");
    }

    implementation::LoadBalancePlan plan;
    plan.nWorkers = nWorkers;
    plan.averageWorkUnitsPerWorker = averageWorkUnitsPerWorker;

    plan.files.reserve(filenames.size());
    for (const auto& filename : filenames) {
        plan.files.push_back(implementation::FileWithSize{filename, static_cast<std::uint64_t>(fileSizeBytes(filename))});
    }

    for (const auto& file : plan.files) {
        plan.totalSizeBytes = implementation::checkedAdd(plan.totalSizeBytes, file.totalSizeBytes, "total input size");
    }

    plan.sizePerWorkerBytes = plan.totalSizeBytes / static_cast<std::uint64_t>(nWorkers);
    plan.referenceWorkUnitSizeBytes = std::max<std::uint64_t>(
        1, plan.sizePerWorkerBytes / static_cast<std::uint64_t>(averageWorkUnitsPerWorker));

    plan.workUnits = implementation::makeEstimatedWorkUnits(plan.files, plan.referenceWorkUnitSizeBytes);
    plan.buckets = implementation::makeBalancedWorkBuckets(plan.workUnits, nWorkers);

    return plan.buckets;
}

std::vector<char> serializeWorkBucket(const WorkBucket& bucket) {
    return implementation::serializeWorkBucketImpl(bucket);
}

WorkBucket deserializeWorkBucket(const std::vector<char>& payload) {
    return implementation::deserializeWorkBucketImpl(payload);
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
