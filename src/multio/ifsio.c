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

#define FDB_LEGACY_TRACE_FUNC fprintf(stdout,"MULTIO %s : %s()\n","0.0.1",__FUNCTION__);

/********************************************************************************************/

int writefdb(void *grib, size_t len)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int set_fdb_root(const char* name)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int setvalfdb(const char* name, const char* value)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int setcommfdb(int comm)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int setrankfdb(int rank)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int setfieldcountfdb(int all_ranks, int this_rank)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int readfdb(void *data, unsigned long long* size)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int openfdb(const char* name, const char* mode)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int initfdb()
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int flushfdb()
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int closefdb()
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int initfdb_vpp(const char* name)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

/********************************************************************************************/

fortint iwritefdb_(fortint* addr, void *data, fortint* words)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

fortint iset_fdb_root_(fortint *addr,const char* name, int name_len)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

fortint isetvalfdb_(fortint *addr,const char* name, const char* value, int name_len, int value_len)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int isetcommfdb_(fortint* comm)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int isetrankfdb_(fortint *addr, fortint* rank)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

int isetfieldcountfdb_(fortint *addr, fortint* all_ranks, fortint* this_rank)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

fortint ireadfdb_(fortint* addr, void *data, fortint* words)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

fortint iopenfdb_(const char* name, fortint* addr, const char* mode, int name_len, int mode_len)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

fortint iinitfdb_()
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

fortint iflushfdb_(fortint *addr)
{
    FDB_LEGACY_TRACE_FUNC

    return 0;
}

fortint iclosefdb_(fortint* addr)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

fortint iinitfdb_vpp_(const char* name, int name_len)
{
    FDB_LEGACY_TRACE_FUNC
    return 0;
}

