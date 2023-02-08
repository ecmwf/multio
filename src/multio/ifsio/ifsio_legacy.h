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

#include <sys/types.h>

typedef int32_t fortint;

extern "C" {

fortint iinitfdb_();
fortint iinitfdb_vpp_(const char* name, int name_len);
fortint iopenfdb_(const char* name, fortint* addr, const char* mode, int name_len, int mode_len);
fortint iclosefdb_(fortint* addr);
fortint iflushfdb_(const fortint* addr);
fortint iwritefdb_(const fortint* addr, const void* data, const fortint* words);
fortint iset_fdb_root_(const fortint* addr, const char* name, int name_len);
fortint isetvalfdb_(const fortint* addr, const char* name, const char* value, int name_len, int value_len);
int isetcommfdb_(const fortint* comm);
int isetrankfdb_(const fortint* addr, const fortint* rank);
int isetfieldcountfdb_(const fortint* addr, const fortint* all_ranks, const fortint* this_rank);
fortint ireadfdb_(const fortint* addr, void* data, fortint* words);

fortint imultio_notify_step_(const fortint* step);

}  // extern C
