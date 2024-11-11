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
 *   @author Tiago Quintino
 *   @date Dec 2015
 */

#include <sys/types.h>

#pragma once

typedef int32_t fortint;

extern "C" {

fortint imultio_flush_();
fortint imultio_flush_last_();
fortint imultio_notify_step_(const fortint* step);
fortint imultio_write_(const void* data, const fortint* words);

// metadata is expected to be a pointer to an eckit::configuration
fortint imultio_write_raw_(const void* metadata, const void* data, const fortint* words);

fortint imultio_encode_bitspervalue_(fortint* bitspervalue, const fortint* paramid, const char* levtype,
                                     const double* min, const double* max, int levtype_len);

}  // extern C
