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
/// @brief MPI wrapper helpers for `grib-to-mtg2`.

#include "multio/tools/grib-to-mtg2/MpiUtils.h"

#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"

namespace multio::grib_to_mtg2 {

namespace {

constexpr std::size_t rootRank = 0;
constexpr int bucketSizeTag = 4000;
constexpr int bucketPayloadTag = 4001;
constexpr int outcomesSizeTag = 5000;
constexpr int outcomesPayloadTag = 5001;

}  // namespace

std::string broadcastOptionsStringFromRoot(const std::string& rootPayload, const eckit::mpi::Comm& comm) {
    std::size_t size = comm.rank() == rootRank ? rootPayload.size() : 0;
    comm.broadcast(size, rootRank);

    std::vector<char> payload;
    if (comm.rank() == rootRank) {
        payload.assign(rootPayload.begin(), rootPayload.end());
    }
    else {
        payload.resize(size);
    }

    if (size > 0) {
        comm.broadcast(payload, rootRank);
    }

    return std::string(payload.begin(), payload.end());
}

WorkBucket distributeRankOwnedBucket(const std::vector<WorkBucket>* rootBuckets, const eckit::mpi::Comm& comm) {
    if (comm.rank() == rootRank) {
        if (rootBuckets == nullptr) {
            throw eckit::SeriousBug("rootBuckets is null on root rank", Here());
        }

        if (rootBuckets->size() != comm.size()) {
            throw eckit::BadValue("bucket count does not match communicator size", Here());
        }

        for (std::size_t rank = 1; rank < comm.size(); ++rank) {
            const auto payload = serializeWorkBucket((*rootBuckets)[rank]);
            const auto payloadSize = payload.size();
            comm.send(payloadSize, static_cast<int>(rank), bucketSizeTag);
            if (payloadSize > 0) {
                comm.send(payload.data(), payload.size(), static_cast<int>(rank), bucketPayloadTag);
            }
        }

        return (*rootBuckets)[rootRank];
    }

    std::size_t payloadSize = 0;
    comm.receive(payloadSize, static_cast<int>(rootRank), bucketSizeTag);

    std::vector<char> payload(payloadSize);
    if (payloadSize > 0) {
        comm.receive(payload.data(), payload.size(), static_cast<int>(rootRank), bucketPayloadTag);
    }

    return deserializeWorkBucket(payload);
}

std::vector<FileStageOutcomes> gatherOutcomes(const std::vector<FileStageOutcomes>& localOutcomes,
                                              const eckit::mpi::Comm& comm) {
    const auto localPayloadString = serializeFileStageOutcomes(localOutcomes);
    const std::vector<char> localPayload(localPayloadString.begin(), localPayloadString.end());

    if (comm.rank() == rootRank) {
        std::vector<FileStageOutcomes> gathered = localOutcomes;

        for (std::size_t rank = 1; rank < comm.size(); ++rank) {
            std::size_t payloadSize = 0;
            comm.receive(payloadSize, static_cast<int>(rank), outcomesSizeTag);

            std::vector<char> payload(payloadSize);
            if (payloadSize > 0) {
                comm.receive(payload.data(), payload.size(), static_cast<int>(rank), outcomesPayloadTag);
            }

            const auto remote = deserializeFileStageOutcomes(std::string(payload.begin(), payload.end()));
            gathered.insert(gathered.end(), remote.begin(), remote.end());
        }

        return gathered;
    }

    const auto payloadSize = localPayload.size();
    comm.send(payloadSize, static_cast<int>(rootRank), outcomesSizeTag);
    if (payloadSize > 0) {
        comm.send(localPayload.data(), localPayload.size(), static_cast<int>(rootRank), outcomesPayloadTag);
    }

    return {};
}

}  // namespace multio::grib_to_mtg2
