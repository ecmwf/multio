
#ifndef multio_util_ConfigurationPath_H
#define multio_util_ConfigurationPath_H

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/utils/Optional.h"

namespace multio {
namespace util {

inline eckit::PathName configuration_path_name() {
    eckit::PathName base = (::getenv("MULTIO_SERVER_CONFIG_PATH"))
                               ? eckit::PathName{::getenv("MULTIO_SERVER_CONFIG_PATH")}
                               : eckit::PathName{""};

    return base + "/";
}

inline eckit::PathName configuration_file_name() {
    const auto configFile = "MULTIO_SERVER_CONFIG_FILE";
    return (::getenv(configFile))
               ? eckit::PathName{::getenv(configFile)}
               : eckit::PathName{configuration_path_name() + "multio-server.yaml"};
}

inline const eckit::LocalConfiguration& configuration_file() {
    static eckit::Optional<eckit::LocalConfiguration> config_;

    if (!config_.has_value()) {
        config_ = eckit::LocalConfiguration(eckit::YAMLConfiguration{configuration_file_name()});
    }
    return config_.value();
}


}  // namespace util
}  // namespace multio

#endif
