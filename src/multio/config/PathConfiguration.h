
#pragma once

#include <cstring>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "multio/util/Environment.h"

namespace multio::config {

struct ConfigPaths {
    eckit::PathName configDir;
    eckit::PathName configFile;
};

inline ConfigPaths defaultConfigPaths(std::optional<eckit::PathName> givenFilePath = std::nullopt) {
    ConfigPaths ret;

    auto path = util::getEnv("MULTIO_SERVER_CONFIG_PATH");
    eckit::PathName base
        = path ? eckit::PathName{std::string{*path}} : (givenFilePath ? givenFilePath->dirName() : eckit::PathName{""});
    ret.configDir = base + "/";

    auto file = util::getEnv("MULTIO_SERVER_CONFIG_FILE");
    ret.configFile = file ? eckit::PathName{std::string{*file}} : eckit::PathName{ret.configDir + "multio-server.yaml"};

    return ret;
}


// Legacy calls - to be removed
inline eckit::PathName configuration_path_name(const eckit::PathName& pathOfFile = "") {
    auto path = util::getEnv("MULTIO_SERVER_CONFIG_PATH");
    eckit::PathName base = path ? eckit::PathName{std::string{*path}} : pathOfFile.dirName();
    return base + "/";
}

inline eckit::PathName configuration_file_name() {
    auto file = util::getEnv("MULTIO_SERVER_CONFIG_FILE");
    return file ? eckit::PathName{std::string{*file}}
                : eckit::PathName{configuration_path_name() + "multio-server.yaml"};
}

inline const eckit::LocalConfiguration& configuration_file() {
    static eckit::LocalConfiguration theconfig{eckit::YAMLConfiguration{configuration_file_name()}};
    return theconfig;
}

}  // namespace multio::config
