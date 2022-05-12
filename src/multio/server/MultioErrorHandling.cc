
#include "MultioErrorHandling.h"

#include "multio/transport/TransportRegistry.h"

int multio_handle_error(const std::exception& e) {

    eckit::Log::error() << "Multio caught exception at C-C++ API boundary:: " << e.what()
                        << std::endl;

    // TODO: should this be a configuration option instead?
    static char* abort_on_error = ::getenv("MULTIO_ABORT_ON_ERROR");
    if (abort_on_error) {
        eckit::Log::info() << "MultIO: MULTIO_ABORT_ON_ERROR is SET -- aborting ... " << std::endl;

        multio::transport::TransportRegistry::instance().abortAll();
    }

    return -2;
}
