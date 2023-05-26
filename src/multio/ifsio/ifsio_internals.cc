/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <stdlib.h>
#include <iostream>

#include "eckit/config/LibEcKit.h"

#include "multio/ifsio/ifsio_internals.h"

bool traceme() {
    static char* trace = ::getenv("MULTIO_TRACE");
    if (trace == nullptr) {
        return false;
    }
    return true;
}

int ifsio_handle_error(std::exception& e) {

    std::cout << "MultIO wrapper: " << e.what() << std::endl << std::flush;
    std::cerr << "MultIO wrapper: " << e.what() << std::endl << std::flush;

    static char* abort_on_error = ::getenv("MULTIO_ABORT_ON_ERROR");
    if (abort_on_error) {
        std::cout << "MultIO wrapper: MULTIO_ABORT_ON_ERROR is SET -- aborting ... " << std::endl << std::flush;
        std::cerr << "MultIO wrapper: MULTIO_ABORT_ON_ERROR is SET -- aborting ... " << std::endl << std::flush;

        eckit::LibEcKit::instance().abort();
    }

    return -2;
}
