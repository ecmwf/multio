#ifndef multio_logfile_name_H
#define multio_logfile_name_H

#include <sstream>
#include <string>
#include <unistd.h>

#include "eckit/runtime/Main.h"

namespace multio {
namespace util {

inline std::string filename_prefix() {
    std::ostringstream os;
    os << "multio-" << eckit::Main::hostname() << "-" << ::getpid();
    return os.str();
}

inline std::string logfile_name() {
    return filename_prefix() + ".log";
}

}  // namespace util
}  // namespace multio

#endif // multio_logfile_name_H
