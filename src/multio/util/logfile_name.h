#ifndef multio_logfile_name_H
#define multio_logfile_name_H

#include <sstream>
#include <string>
#include <unistd.h>

#include "eckit/runtime/Main.h"

namespace multio {
namespace util {

inline std::string logfile_name() {
    std::ostringstream os;
    os << "multio-" << eckit::Main::hostname() << "-" << ::getpid() << ".log";
    return os.str();
}

}  // namespace util
}  // namespace multio

#endif // multio_logfile_name_H
