
#ifndef multio_util_ConfigurationPath_H
#define multio_util_ConfigurationPath_H

#include "eckit/filesystem/PathName.h"

namespace multio {
namespace util {

inline eckit::PathName configuration_path() {
    eckit::PathName base = (::getenv("MULTIO_SERVER_CONFIG_PATH"))
                               ? eckit::PathName{::getenv("MULTIO_SERVER_CONFIG_PATH")}
                               : eckit::PathName{""};

    return base + "/";
}

inline eckit::PathName configuration_file() {
    const auto configFile = "MULTIO_SERVER_CONFIG_FILE";
    return (::getenv(configFile)) ? eckit::PathName{::getenv(configFile)}
                                  : eckit::PathName{configuration_path() + "multio-server.yaml"};
}

}  // namespace util
}  // namespace multio

#endif
