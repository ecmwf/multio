
#ifndef multio_server_ConfigurationPath_H
#define multio_server_ConfigurationPath_H

#include "eckit/filesystem/PathName.h"

inline eckit::PathName configuration_path() {
    eckit::PathName base = (::getenv("MULTIO_SERVER_PATH"))
                               ? eckit::PathName{::getenv("MULTIO_SERVER_PATH")}
                               : eckit::PathName{""};

    return base + "/configs/";
}

#endif
