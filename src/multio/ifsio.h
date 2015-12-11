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
 *   @date Dec 2013
 */

#include <sys/types.h>

#ifndef multio_ifsio_h
#define multio_ifsio_h

typedef int fortint;

/* Legacy C interface */

int writefdb(void *data, size_t len);
int set_fdb_root(const char *name);
int setvalfdb(const char *name, const char *value);
int setcommfdb(int rank);
int setrankfdb(int rank);
int setfieldcountfdb(int all_ranks, int this_rank);
int readfdb(void *data, unsigned long long *size);
int openfdb(const char *name, const char *mode);
int initfdb();
int flushfdb();
int closefdb();
int initfdb_vpp(const char *name);

/* Legacy Fortran interface */

fortint iwritefdb_(fortint *addr, void *data, fortint *words);
fortint iset_fdb_root_(fortint *addr, const char *name, int name_len);
fortint isetvalfdb_(fortint *addr, const char *name, const char *value, int name_len, int value_len);
int isetcommfdb_(fortint *rank);
int isetrankfdb_(fortint *addr, fortint *rank);
int isetfieldcountfdb_(fortint *addr, fortint *all_ranks, fortint *this_rank);
fortint ireadfdb_(fortint *addr, void *data, fortint *words);
fortint iopenfdb_(const char *name, fortint *addr, const char *mode, int name_len, int mode_len);
fortint iinitfdb_(void);
fortint iflushfdb_(fortint *addr);
fortint iclosefdb_(fortint *addr);
fortint iinitfdb_vpp_(const char *name, int name_len);

#endif
