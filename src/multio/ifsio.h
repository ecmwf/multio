/*
 * (C) Copyright 1996-2013 ECMWF.
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

#ifndef multio_ifsio_h
#define multio_ifsio_h

#ifdef __cplusplus
extern "C" {
#endif

/* Legacy Fortran interface */

typedef int fortint;

fortint iinitfdb_(void);

fortint iopenfdb_(const char *name, fortint *addr, const char *mode, int name_len, int mode_len);
fortint iclosefdb_(fortint *addr);

fortint iflushfdb_(fortint *addr);

fortint iset_fdb_root_(fortint *addr, const char *name, int name_len);

int isetcommfdb_(fortint *rank);
int isetrankfdb_(fortint *addr, fortint *rank);
int isetfieldcountfdb_(fortint *addr, fortint *all_ranks, fortint *this_rank);

fortint isetvalfdb_(fortint *addr, const char *name, const char *value, int name_len, int value_len);

fortint iwritefdb_(fortint *addr, void *data, fortint *words);
fortint ireadfdb_(fortint *addr, void *data, fortint *words);

#ifdef __cplusplus
}
#endif

#endif
