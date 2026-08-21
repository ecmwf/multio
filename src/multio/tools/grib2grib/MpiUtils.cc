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
/// @brief MPI wrapper helpers for `grib2grib`.

#include "multio/tools/grib2grib/MpiUtils.h"

#include <limits>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

constexpr std::size_t rootRank = 0;
constexpr int bucketSizeTag = 4000;
constexpr int bucketPayloadTag = 4001;
constexpr int outcomesSizeTag = 5000;
constexpr int outcomesPayloadTag = 5001;

}  // namespace

std::string broadcastOptionsStringFromRoot(const std::string& rootPayload, const eckit::mpi::Comm& comm) {
    std::uint64_t size = comm.rank() == rootRank ? static_cast<std::uint64_t>(rootPayload.size()) : 0;
    comm.broadcast(size, rootRank);

    if (size > static_cast<std::uint64_t>(std::numeric_limits<std::size_t>::max())) {
        throw eckit::BadValue("broadcast payload too large for std::string", Here());
    }

    std::vector<char> payload;
    if (comm.rank() == rootRank) {
        payload.assign(rootPayload.begin(), rootPayload.end());
    }
    else {
        payload.resize(static_cast<std::size_t>(size));
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
            const auto payloadSize = static_cast<std::uint64_t>(payload.size());
            comm.send(payloadSize, static_cast<int>(rank), bucketSizeTag);
            if (payloadSize > 0) {
                comm.send(payload.data(), payload.size(), static_cast<int>(rank), bucketPayloadTag);
            }
        }

        return (*rootBuckets)[rootRank];
    }

    std::uint64_t payloadSize = 0;
    comm.receive(payloadSize, static_cast<int>(rootRank), bucketSizeTag);

    if (payloadSize > static_cast<std::uint64_t>(std::numeric_limits<std::size_t>::max())) {
        throw eckit::BadValue("incoming bucket payload too large", Here());
    }

    std::vector<char> payload(static_cast<std::size_t>(payloadSize));
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
            std::uint64_t payloadSize = 0;
            comm.receive(payloadSize, static_cast<int>(rank), outcomesSizeTag);

            if (payloadSize > static_cast<std::uint64_t>(std::numeric_limits<std::size_t>::max())) {
                throw eckit::BadValue("incoming outcomes payload too large", Here());
            }

            std::vector<char> payload(static_cast<std::size_t>(payloadSize));
            if (payloadSize > 0) {
                comm.receive(payload.data(), payload.size(), static_cast<int>(rank), outcomesPayloadTag);
            }

            const auto remote = deserializeFileStageOutcomes(std::string(payload.begin(), payload.end()));
            gathered.insert(gathered.end(), remote.begin(), remote.end());
        }

        return gathered;
    }

    const auto payloadSize = static_cast<std::uint64_t>(localPayload.size());
    comm.send(payloadSize, static_cast<int>(rootRank), outcomesSizeTag);
    if (payloadSize > 0) {
        comm.send(localPayload.data(), localPayload.size(), static_cast<int>(rootRank), outcomesPayloadTag);
    }

    return {};
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
