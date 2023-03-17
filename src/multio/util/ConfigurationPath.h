
#pragma once

#include <cstring>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "multio/util/Environment.h"

namespace multio {
namespace util {

inline eckit::PathName configuration_path_name(const eckit::PathName& pathOfFile = "") {
    auto path = getEnv("MULTIO_SERVER_CONFIG_PATH");
    eckit::PathName base = path  ? eckit::PathName{std::string{*path}} : pathOfFile.dirName();
    return base + "/";
}

inline eckit::PathName configuration_file_name() {
    auto file = getEnv("MULTIO_SERVER_CONFIG_FILE");
    return file ? eckit::PathName{std::string{*file}} : eckit::PathName{configuration_path_name() + "multio-server.yaml"};
}

inline const eckit::LocalConfiguration& configuration_file() {
    static eckit::LocalConfiguration theconfig{eckit::YAMLConfiguration{configuration_file_name()}};
    return theconfig;
}


}  // namespace util
}  // namespace multio
