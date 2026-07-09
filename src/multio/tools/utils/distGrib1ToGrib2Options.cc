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
#include "eckit/filesystem/PathName.h"
#include "eckit/io/SharedBuffer.h"

namespace multio::distGrib1ToGrib2 {

namespace {

std::string readOptionsFile(const std::string& path) {
    std::ifstream in(path);
    if (!in) {
        throw std::runtime_error("cannot open options file: " + path);
    }

    std::ostringstream os;
    os << in.rdbuf();

    if (!in.good() && !in.eof()) {
        throw std::runtime_error("error while reading options file: " + path);
    }

    return os.str();
}

void normalizeOptions(eckit::LocalConfiguration& options) {
    if (options.has("verbose") && options.getBool("verbose") && !options.has("verbosity")) {
        options.set("verbosity", 2L);
    }
}

eckit::LocalConfiguration parseOptionsYaml(const std::string& payload) {
    if (payload.empty()) {
        throw std::runtime_error("empty options payload");
    }

    std::istringstream in(payload);

    eckit::YAMLConfiguration yaml(in);
    eckit::LocalConfiguration options(yaml);

    normalizeOptions(options);

    return options;
}

}  // namespace

eckit::LocalConfiguration loadOptionsFromYamlFile(const std::string& yamlFile) {
    return parseOptionsYaml(readOptionsFile(yamlFile));
}

eckit::LocalConfiguration loadAndBroadcastOptions(const std::string& yamlFile, const eckit::mpi::Comm& comm) {
    eckit::SharedBuffer buf = comm.broadcastFile(eckit::PathName{yamlFile}, /*root=*/0);

    if (buf.size() == 0) {
        throw std::runtime_error("empty options payload broadcast from root");
    }

    eckit::YAMLConfiguration yaml(buf);
    eckit::LocalConfiguration options(yaml);
    normalizeOptions(options);

    return options;
}

std::string debugOutputPrefix(const eckit::LocalConfiguration& options) {
    if (!options.has("debug")) {
        return {};
    }
    const auto debugConf = options.getSubConfiguration("debug");
    if (!debugConf.has("output-prefix")) {
        return {};
    }
    return debugConf.getString("output-prefix");
}

}  // namespace multio::distGrib1ToGrib2
