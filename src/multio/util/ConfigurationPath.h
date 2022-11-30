
#ifndef multio_util_ConfigurationPath_H
#define multio_util_ConfigurationPath_H

#include "eckit/config/Resource.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/utils/Optional.h"

namespace multio {
namespace util {

inline eckit::PathName configuration_path_name(const eckit::PathName& pathOfFile = "") {
    // TODO We should use resource, but this changes the current default behaviour
    // static eckit::PathName basepath(eckit::Resource<eckit::PathName>("$MULTIO_SERVER_CONFIG_PATH", "~multio/etc"));
    // return basepath;
    
    eckit::PathName base = (::getenv("MULTIO_SERVER_CONFIG_PATH"))
                             ? eckit::PathName{::getenv("MULTIO_SERVER_CONFIG_PATH")}
                             : pathOfFile.dirName();

    return base + "/";
}

inline eckit::PathName configuration_file_name() {
    const auto configFile = "MULTIO_SERVER_CONFIG_FILE";
    return (::getenv(configFile)) ? eckit::PathName{::getenv(configFile)}
                                  : eckit::PathName{configuration_path_name() + "multio-server.yaml"};
}

inline const eckit::LocalConfiguration& configuration_file() {
    static eckit::LocalConfiguration theconfig{eckit::YAMLConfiguration{configuration_file_name()}};
    return theconfig;
}


}  // namespace util
}  // namespace multio

#endif
