/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include <string>
#include <vector>

#include "eckit/mpi/Comm.h"

namespace multio::distGrib1ToGrib2 {

std::string serializeFileList(const std::vector<std::string>& files);
std::vector<std::string> deserializeFileList(const std::string& payload);
void sendFileListToRank(const std::vector<std::string>& files, int dest, const eckit::mpi::Comm& comm);
std::vector<std::string> recvFileListFromRank0(const eckit::mpi::Comm& comm);

std::string broadcastStringFromRoot(const std::string& rootPayload, int rank, const eckit::mpi::Comm& comm);
std::string gatherStringToRank0(const std::string& local, int rank, int worldSize, const eckit::mpi::Comm& comm);

}  // namespace multio::distGrib1ToGrib2
