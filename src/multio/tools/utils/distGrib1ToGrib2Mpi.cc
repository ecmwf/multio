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

void mpiSendString(const std::string& s, int dest, int tag, MPI_Comm comm) {
    const unsigned long long n = static_cast<unsigned long long>(s.size());
    MPI_Send(&n, 1, MPI_UNSIGNED_LONG_LONG, dest, tag, comm);

    const char* ptr = s.data();
    unsigned long long remaining = n;
    while (remaining > 0) {
        const int chunkSize = static_cast<int>(std::min<unsigned long long>(remaining, static_cast<unsigned long long>(INT_MAX)));
        MPI_Send(ptr, chunkSize, MPI_CHAR, dest, tag + 1, comm);
        ptr += chunkSize;
        remaining -= static_cast<unsigned long long>(chunkSize);
    }
}

std::string mpiRecvString(int source, int tag, MPI_Comm comm) {
    unsigned long long n = 0;
    MPI_Recv(&n, 1, MPI_UNSIGNED_LONG_LONG, source, tag, comm, MPI_STATUS_IGNORE);

    if (n > static_cast<unsigned long long>(std::numeric_limits<std::size_t>::max())) {
        throw std::runtime_error("incoming MPI string too large for std::string");
    }

    std::string s;
    s.resize(static_cast<std::size_t>(n));

    char* ptr = s.data();
    unsigned long long remaining = n;
    while (remaining > 0) {
        const int chunkSize = static_cast<int>(std::min<unsigned long long>(remaining, static_cast<unsigned long long>(INT_MAX)));
        MPI_Recv(ptr, chunkSize, MPI_CHAR, source, tag + 1, comm, MPI_STATUS_IGNORE);
        ptr += chunkSize;
        remaining -= static_cast<unsigned long long>(chunkSize);
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

void sendFileListToRank(const std::vector<std::string>& files, int dest, MPI_Comm comm) {
    mpiSendString(serializeFileList(files), dest, 1000, comm);
}

std::vector<std::string> recvFileListFromRank0(MPI_Comm comm) {
    return deserializeFileList(mpiRecvString(0, 1000, comm));
}

std::string broadcastStringFromRoot(const std::string& rootPayload, int rank, MPI_Comm comm) {
    unsigned long long n = rank == 0 ? static_cast<unsigned long long>(rootPayload.size()) : 0;
    MPI_Bcast(&n, 1, MPI_UNSIGNED_LONG_LONG, 0, comm);

    if (n > static_cast<unsigned long long>(std::numeric_limits<std::size_t>::max())) {
        throw std::runtime_error("broadcast payload too large for std::string");
    }

    std::string payload = rank == 0 ? rootPayload : std::string(static_cast<std::size_t>(n), '\0');
    if (n > 0) {
        MPI_Bcast(payload.data(), static_cast<int>(n), MPI_CHAR, 0, comm);
    }
    return payload;
}

std::string gatherStringToRank0(const std::string& local, int rank, int worldSize, MPI_Comm comm) {
    if (local.size() > static_cast<std::size_t>(INT_MAX)) {
        throw std::runtime_error("local gather payload larger than INT_MAX");
    }

    const int localSize = static_cast<int>(local.size());
    std::vector<int> recvCounts;
    std::vector<int> displs;
    if (rank == 0) {
        recvCounts.resize(static_cast<std::size_t>(worldSize));
    }

    MPI_Gather(&localSize, 1, MPI_INT, rank == 0 ? recvCounts.data() : nullptr, 1, MPI_INT, 0, comm);

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

    MPI_Gatherv(local.data(), localSize, MPI_CHAR, rank == 0 ? global.data() : nullptr,
                rank == 0 ? recvCounts.data() : nullptr, rank == 0 ? displs.data() : nullptr, MPI_CHAR, 0, comm);

    return global;
}

}  // namespace multio::distGrib1ToGrib2
