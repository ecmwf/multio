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

#ifdef __cplusplus
extern "C" {
#endif

void multio_open_connections();

void multio_close_connections();

void multio_write_step_complete();

int multio_init_client(const char* name, int parent_comm);

void multio_init_server(int parent_comm);

void multio_metadata_set_int_value(const char* key, int value);

void multio_metadata_set_string_value(const char* key, const char* value);

void multio_set_domain(const char* key, int* data, int size);

void multio_write_field(const char* fname, const double* data, int size, bool to_all_servers);

bool multio_field_is_active(const char* fname);

void multio_not_implemented(const char* message);

#ifdef __cplusplus
}
#endif

#endif
