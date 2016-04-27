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
#include <string.h>

#include "eckit/exception/Exceptions.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/config/Resource.h"
#include "eckit/config/JSONConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "multio/multio_version.h"
#include "multio/ifsio.h"

#include "multio/MultIO.h"

#include "gribpp/GribDataBlob.h"

using namespace eckit;
using namespace multio;

static multio::MultIO* mio = 0;
static fortint fdbAddr = 0;
static eckit::Mutex *local_mutex = 0;

static pthread_once_t once = PTHREAD_ONCE_INIT;

static void init() {

    local_mutex = new eckit::Mutex();

    PathName multioConfigFile = Resource<PathName>("multioConfigFile;$MULTIO_CONFIG_FILE", "multio.json");
    eckit::JSONConfiguration config(multioConfigFile);

    mio = new MultIO(config);
}

/**********************************************************************************************************************/

#define MULTIO_TRACE fprintf(stdout,"MULTIO %s : %s()\n",MULTIO_VERSION,__FUNCTION__);

#ifdef  MULTIO_TRACE
#define MULTIO_TRACE_FUNC()       fprintf(stdout,"MULTIO %s : %s()\n",MULTIO_VERSION,__FUNCTION__);
#define MULTIO_TRACE_FUNC1(p1)    fprintf(stdout,"MULTIO %s : %s(%s)\n",MULTIO_VERSION,__FUNCTION__,p1);
#define MULTIO_TRACE_FUNC2(p1,p2) fprintf(stdout,"MULTIO %s : %s(%s,%s)\n",MULTIO_VERSION,__FUNCTION__,p1,p2);
#else
#define MULTIO_TRACE_FUNC()
#define MULTIO_TRACE_FUNC1(p1)
#define MULTIO_TRACE_FUNC2(p1,p2)
#endif

/**********************************************************************************************************************/

fortint iinitfdb_() {

    try {

        pthread_once(&once, init);
        eckit::AutoLock<eckit::Mutex> lock(local_mutex);

        ASSERT(mio);

        return mio->iinitfdb();
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

fortint iopenfdb_(const char* name, fortint* addr, const char* mode, int name_len, int mode_len) {

    try {


        pthread_once(&once, init);
        eckit::AutoLock<eckit::Mutex> lock(local_mutex);

        ASSERT(mio);

        fortint res  = mio->iopenfdb(name, mode, name_len, mode_len);
        *addr = 1;
        fdbAddr = *addr;

        return res;
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

fortint iclosefdb_(fortint* addr) {

    try {


        pthread_once(&once, init);
        eckit::AutoLock<eckit::Mutex> lock(local_mutex);

        ASSERT(mio && fdbAddr == *addr);

        mio->iclosefdb();

        *addr = 0;
        fdbAddr = 0;

        return 0;
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

fortint iflushfdb_(fortint *addr) {

    try {


        ASSERT(mio && fdbAddr == *addr);

        return mio->iflushfdb();
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

fortint iwritefdb_(const fortint* addr, const void *data, const fortint* words) {

    try {


        ASSERT(mio && fdbAddr == *addr);

        size_t len( (*words)*sizeof(fortint) );

        eckit::DataBlobPtr blob ( new gribpp::GribDataBlob(data, len) );
        mio->write(blob);
        return 0;
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

fortint iset_fdb_root_(fortint *addr,const char* name, int name_len) {

    try {


        pthread_once(&once, init);
        eckit::AutoLock<eckit::Mutex> lock(local_mutex);

        ASSERT(mio && fdbAddr == *addr);

        return mio->iset_fdb_root(name, name_len);
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

fortint isetvalfdb_(fortint *addr, const char* name, const char* value, int name_len, int value_len) {

    try {


        ASSERT(mio && fdbAddr == *addr);

        return mio->isetvalfdb(name, value, name_len, value_len);
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

int isetcommfdb_(fortint* comm) {

    try {


        pthread_once(&once, init);
        eckit::AutoLock<eckit::Mutex> lock(local_mutex);

        ASSERT(mio);

        return mio->isetcommfdb(comm);
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

int isetrankfdb_(fortint *addr, fortint* rank) {

    try {

        ASSERT(mio && fdbAddr == *addr);

        return mio->isetrankfdb(rank);
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

int isetfieldcountfdb_(fortint *addr, fortint* all_ranks, fortint* this_rank)
{

    try {

        ASSERT(mio && fdbAddr == *addr);

        return mio->isetfieldcountfdb(all_ranks, this_rank);
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}

fortint ireadfdb_(fortint* addr, void *data, fortint* words)
{

    try {

        NOTIMP;
        return 0;
    }
    catch (std::exception &e) {
        eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
        return -2;
    }
    return 0;
}


