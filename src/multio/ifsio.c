/*
 * (C) Copyright 1996-2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <stdio.h>
#include <sys/types.h>

#include "multio/multio_version.h"
#include "ifsio.h"

/********************************************************************************************/

#define MULTIO_TRACE fprintf(stdout,"MULTIO %s : %s()\n",MULTIO_VERSION,__FUNCTION__);

/********************************************************************************************/

int writefdb(void *grib, size_t len)
{
    MULTIO_TRACE
    return 0;
}

int set_fdb_root(const char* name)
{
    MULTIO_TRACE
    return 0;
}

int setvalfdb(const char* name, const char* value)
{
    MULTIO_TRACE
    return 0;
}

int setcommfdb(int comm)
{
    MULTIO_TRACE
    return 0;
}

int setrankfdb(int rank)
{
    MULTIO_TRACE
    return 0;
}

int setfieldcountfdb(int all_ranks, int this_rank)
{
    MULTIO_TRACE
    return 0;
}

int readfdb(void *data, unsigned long long* size)
{
    MULTIO_TRACE
    return 0;
}

int openfdb(const char* name, const char* mode)
{
    MULTIO_TRACE
    return 0;
}

int initfdb()
{
    MULTIO_TRACE
    return 0;
}

int flushfdb()
{
    MULTIO_TRACE
    return 0;
}

int closefdb()
{
    MULTIO_TRACE
    return 0;
}

int initfdb_vpp(const char* name)
{
    MULTIO_TRACE
    return 0;
}

/********************************************************************************************/

fortint iwritefdb_(fortint* addr, void *data, fortint* words)
{
    MULTIO_TRACE
    return 0;
}

fortint iset_fdb_root_(fortint *addr,const char* name, int name_len)
{
    MULTIO_TRACE
    return 0;
}

fortint isetvalfdb_(fortint *addr,const char* name, const char* value, int name_len, int value_len)
{
    MULTIO_TRACE
    return 0;
}

int isetcommfdb_(fortint* comm)
{
    MULTIO_TRACE
    return 0;
}

int isetrankfdb_(fortint *addr, fortint* rank)
{
    MULTIO_TRACE
    return 0;
}

int isetfieldcountfdb_(fortint *addr, fortint* all_ranks, fortint* this_rank)
{
    MULTIO_TRACE
    return 0;
}

fortint ireadfdb_(fortint* addr, void *data, fortint* words)
{
    MULTIO_TRACE
    return 0;
}

fortint iopenfdb_(const char* name, fortint* addr, const char* mode, int name_len, int mode_len)
{
    MULTIO_TRACE
    return 0;
}

fortint iinitfdb_()
{
    MULTIO_TRACE
    return 0;
}

fortint iflushfdb_(fortint *addr)
{
    MULTIO_TRACE

    return 0;
}

fortint iclosefdb_(fortint* addr)
{
    MULTIO_TRACE
    return 0;
}

fortint iinitfdb_vpp_(const char* name, int name_len)
{
    MULTIO_TRACE
    return 0;
}

