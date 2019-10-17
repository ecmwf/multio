/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_server_MultioNemo_H
#define multio_server_MultioNemo_H

#include <cstdint>

using fortint = int32_t;

#ifdef __cplusplus
extern "C" {
#endif

void multio_set_dimensions_(fortint* clients, fortint* servers, fortint* glfields);

void multio_open_connection_();

void multio_close_connection_();

void multio_send_step_complete_();

void multio_set_domain_(const char* key, fortint* data, fortint* size, fortint key_len);

void multio_write_field_(const char* fname, const double* data, fortint* timeStep, fortint fn_len,
                         fortint data_sz);

#ifdef __cplusplus
}
#endif

#endif
