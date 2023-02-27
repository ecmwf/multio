
#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

namespace multio {
namespace util {

inline eckit::PathName configuration_path_name(const eckit::PathName& pathOfFile = "") {
    char * path = std::getenv("MULTIO_SERVER_CONFIG_PATH");
    eckit::PathName base = (path != NULL && (std::strlen(path) > 0))  ? eckit::PathName{path} : pathOfFile.dirName();
    return base + "/";
}

inline eckit::PathName configuration_file_name() {
    char * file = std::getenv("MULTIO_SERVER_CONFIG_FILE");
    return (file != NULL && (std::strlen(file) > 0)) ? eckit::PathName{file} : eckit::PathName{configuration_path_name() + "multio-server.yaml"};
}

inline const eckit::LocalConfiguration& configuration_file() {
    static eckit::LocalConfiguration theconfig{eckit::YAMLConfiguration{configuration_file_name()}};
    return theconfig;
}


}  // namespace util
}  // namespace multio
