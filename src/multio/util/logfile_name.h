#pragma once

#include <unistd.h>
#include <sstream>
#include <string>

#include "eckit/runtime/Main.h"

namespace multio::util {

inline std::string filename_prefix() {
    std::ostringstream os;
    os << "multio-" << eckit::Main::hostname() << "-" << ::getpid();
    return os.str();
}

inline std::string logfile_name() {
    return filename_prefix() + ".log";
}

}  // namespace multio::util
