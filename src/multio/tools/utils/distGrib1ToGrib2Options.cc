/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/distGrib1ToGrib2Options.h"

#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>

#include "eckit/config/YAMLConfiguration.h"

#include "multio/tools/utils/distGrib1ToGrib2Mpi.h"

namespace multio::distGrib1ToGrib2 {

namespace {

std::string readYamlFile(const std::string& path) {
    std::ifstream in(path);
    if (!in) {
        throw std::runtime_error("cannot open options file: " + path);
    }

    std::ostringstream buffer;
    buffer << in.rdbuf();
    if (!in.good() && !in.eof()) {
        throw std::runtime_error("error while reading options file: " + path);
    }
    return buffer.str();
}

eckit::LocalConfiguration parseOptions(const std::string& payload) {
    eckit::LocalConfiguration options{eckit::YAMLConfiguration{payload.empty() ? std::string{"{}\n"} : payload}};
    if (options.has("verbose") && options.getBool("verbose") && !options.has("verbosity")) {
        options.set("verbosity", 2L);
    }
    return options;
}

}  // namespace

eckit::LocalConfiguration loadAndBroadcastOptions(int rank, const std::optional<std::string>& yamlFile, MPI_Comm comm) {
    const std::string rootPayload = (rank == 0 && yamlFile) ? readYamlFile(*yamlFile) : std::string{"{}\n"};
    const std::string payload = broadcastStringFromRoot(rootPayload, rank, comm);
    return parseOptions(payload);
}

}  // namespace multio::distGrib1ToGrib2
