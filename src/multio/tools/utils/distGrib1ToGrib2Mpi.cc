/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/distGrib1ToGrib2Mpi.h"

#include <algorithm>
#include <climits>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>

namespace multio::distGrib1ToGrib2 {

namespace {

// Wire type for the size prefix. Uses signed long long because eckit::mpi::Data::Type
// specialises Type<long long> (mapping to MPI_LONG_LONG) but not Type<unsigned long long>.
// A signed 64-bit integer is more than sufficient for any real string payload.
using WireSize = long long;

WireSize toWireSize(std::size_t sz) {
    if (sz > static_cast<std::size_t>(std::numeric_limits<WireSize>::max())) {
        throw std::runtime_error("MPI string payload too large for wire size type");
    }
    return static_cast<WireSize>(sz);
}

std::size_t toSizeT(WireSize n) {
    if (n < 0) {
        throw std::runtime_error("negative MPI string size received");
    }
    if (static_cast<unsigned long long>(n) > static_cast<unsigned long long>(std::numeric_limits<std::size_t>::max())) {
        throw std::runtime_error("incoming MPI string too large for std::string");
    }
    return static_cast<std::size_t>(n);
}

void mpiSendString(const std::string& s, int dest, int tag, const eckit::mpi::Comm& comm) {
    const WireSize n = toWireSize(s.size());
    comm.send(&n, 1, dest, tag);

    const char* ptr = s.data();
    WireSize remaining = n;
    while (remaining > 0) {
        const std::size_t chunkSize
            = static_cast<std::size_t>(std::min<WireSize>(remaining, static_cast<WireSize>(INT_MAX)));
        comm.send(ptr, chunkSize, dest, tag + 1);
        ptr += chunkSize;
        remaining -= static_cast<WireSize>(chunkSize);
    }
}

std::string mpiRecvString(int source, int tag, const eckit::mpi::Comm& comm) {
    WireSize n = 0;
    comm.receive(&n, 1, source, tag);

    const std::size_t total = toSizeT(n);

    std::string s;
    s.resize(total);

    char* ptr = s.data();
    WireSize remaining = n;
    while (remaining > 0) {
        const std::size_t chunkSize
            = static_cast<std::size_t>(std::min<WireSize>(remaining, static_cast<WireSize>(INT_MAX)));
        comm.receive(ptr, chunkSize, source, tag + 1);
        ptr += chunkSize;
        remaining -= static_cast<WireSize>(chunkSize);
    }

    return s;
}

}  // namespace

std::string serializeFileList(const std::vector<std::string>& files) {
    std::string payload;
    for (const auto& file : files) {
        payload += file;
        payload += '\n';
    }
    return payload;
}

std::vector<std::string> deserializeFileList(const std::string& payload) {
    std::vector<std::string> files;
    std::istringstream in(payload);
    std::string line;
    while (std::getline(in, line)) {
        if (!line.empty()) {
            files.push_back(std::move(line));
        }
    }
    return files;
}

void sendFileListToRank(const std::vector<std::string>& files, int dest, const eckit::mpi::Comm& comm) {
    mpiSendString(serializeFileList(files), dest, 1000, comm);
}

std::vector<std::string> recvFileListFromRank0(const eckit::mpi::Comm& comm) {
    return deserializeFileList(mpiRecvString(0, 1000, comm));
}

std::string broadcastStringFromRoot(const std::string& rootPayload, int rank, const eckit::mpi::Comm& comm) {
    WireSize n = (rank == 0) ? toWireSize(rootPayload.size()) : WireSize{0};
    comm.broadcast(&n, 1, /*root=*/0);

    const std::size_t total = toSizeT(n);
    std::string payload = (rank == 0) ? rootPayload : std::string(total, '\0');
    if (n > 0) {
        comm.broadcast(payload.data(), total, /*root=*/0);
    }
    return payload;
}

std::string gatherStringToRank0(const std::string& local, int rank, int worldSize, const eckit::mpi::Comm& comm) {
    if (local.size() > static_cast<std::size_t>(INT_MAX)) {
        throw std::runtime_error("local gather payload larger than INT_MAX");
    }

    const int localSize = static_cast<int>(local.size());

    // Allocate recvCounts on every rank so the eckit::mpi::Comm::gather scalar overload accepts it.
    // On non-root ranks the receive buffer is ignored by MPI, but the API requires the correct size.
    std::vector<int> recvCounts(static_cast<std::size_t>(worldSize));

    comm.gather(localSize, recvCounts, /*root=*/0);

    std::vector<int> displs;
    std::string global;
    if (rank == 0) {
        displs.resize(static_cast<std::size_t>(worldSize));
        long long totalSize64 = 0;
        for (int i = 0; i < worldSize; ++i) {
            displs[static_cast<std::size_t>(i)] = static_cast<int>(totalSize64);
            totalSize64 += recvCounts[static_cast<std::size_t>(i)];
            if (totalSize64 > static_cast<long long>(INT_MAX)) {
                throw std::runtime_error("global gather payload larger than INT_MAX");
            }
        }
        global.resize(static_cast<std::size_t>(totalSize64));
    }

    comm.gatherv(local.data(), static_cast<std::size_t>(localSize), rank == 0 ? global.data() : nullptr,
                 rank == 0 ? recvCounts.data() : nullptr, rank == 0 ? displs.data() : nullptr, /*root=*/0);

    return global;
}

}  // namespace multio::distGrib1ToGrib2
