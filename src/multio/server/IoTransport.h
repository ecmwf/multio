/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date May 2019

#ifndef multio_server_IoTransport_H
#define multio_server_IoTransport_H

#include "eccodes.h"

using fortint = int32_t;

#ifdef __cplusplus
extern "C" {
#endif

fortint print_girb_id_(int* gid);
fortint print_grib_handle_(grib_handle* h);

fortint open_io_connection_();
fortint close_io_connection_();
fortint send_grib_template_(const void* grib_msg, int* words);

#ifdef __cplusplus
}
#endif

#endif
