#include <unistd.h>
#include "eckit/mpi/Comm.h"
#include "eckit/exception/Exceptions.h"

#include "multio/api/c/multio_c.h"

void handleError(int error) {
    if (error != 0) {
        exit(error);
    }
}

int main(int argc, char** argv) {
    multio_handle_t* multio_handle = nullptr;
    multio_configuration_t* multio_cc = nullptr;

    handleError(multio_initialise());

    handleError(multio_new_configuration(&multio_cc));

    handleError(multio_new_handle(&multio_handle, multio_cc));

    handleError(multio_delete_configuration(multio_cc));

    handleError(multio_open_connections(multio_handle));

    for (size_t i = 1; i < argc; ++i) {
        multio_metadata_t* md = nullptr;
        handleError(multio_new_metadata(&md, multio_handle));
        handleError(multio_metadata_set_int(md, "step", std::stol(argv[i])));
        handleError(multio_metadata_set_int(md, "timeStep", 3600));
        handleError(multio_synchronize(multio_handle, md));
        handleError(multio_delete_metadata(md));
    }

    handleError(multio_close_connections(multio_handle));

    eckit::mpi::comm().barrier();  // Needed to match the barrier in `multio-probe --test`

    handleError(multio_delete_handle(multio_handle));
}
