/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/**
 *   @author Olivier Iffrig
 *   @date Nov 2019
 */

#pragma once

#include <stdio.h>
#include <exception>

#include "multio/multio_version.h"

bool traceme();

#define MULTIO_TRACE

#ifdef MULTIO_TRACE
#define MULTIO_TRACE_FUNC()                                                \
    if (traceme()) {                                                       \
        fprintf(stdout, "MULTIO %s : %s()\n", multio_version(), __func__); \
    }
#define MULTIO_TRACE_FUNC1(p1)                                                   \
    if (traceme()) {                                                             \
        fprintf(stdout, "MULTIO %s : %s(%s)\n", multio_version(), __func__, p1); \
    }
#define MULTIO_TRACE_FUNC2(p1, p2)                                                      \
    if (traceme()) {                                                                    \
        fprintf(stdout, "MULTIO %s : %s(%s,%s)\n", multio_version(), __func__, p1, p2); \
    }
#else
#define MULTIO_TRACE_FUNC()
#define MULTIO_TRACE_FUNC1(p1)
#define MULTIO_TRACE_FUNC2(p1, p2)
#endif

//----------------------------------------------------------------------------------------------------------------------

int ifsio_handle_error(std::exception& e);
