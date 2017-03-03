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

#include "eckit/runtime/Main.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/config/Resource.h"
#include "eckit/config/JSONConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/parser/Tokenizer.h"

#include "multio/multio_version.h"
#include "multio/ifsio.h"

#include "multio/MultIO.h"

#include "metkit/grib/GribDataBlob.h"

typedef int32_t fortint;

using namespace eckit;
using namespace multio;

static eckit::Mutex *local_mutex = 0;

static pthread_once_t once = PTHREAD_ONCE_INIT;


class MIO {
public:

    static void initialise(const eckit::JSONConfiguration& config) {
        eckit::ScopedPtr<MultIO>& mio(ptr());
        ASSERT(!mio);
        mio.reset(new MultIO(config));
    }

    static MultIO& instance() {
        const eckit::ScopedPtr<MultIO>& mio(ptr());
        ASSERT(mio);
        return *mio;
    }

private:

    static eckit::ScopedPtr<MultIO>& ptr() {
        static eckit::ScopedPtr<MultIO> mio;
        return mio;
    }
};

static void init() {

    local_mutex = new eckit::Mutex();

    static const char *argv[2] = {"ifsio", 0};

    eckit::Main::initialise(1, const_cast<char**>(argv));

    if (::getenv("MULTIO_CONFIG_FILE")) {

        PathName path(::getenv("MULTIO_CONFIG_FILE"));

        std::cout << "MultIO initialising with file " << path << std::endl;

        eckit::JSONConfiguration config(path);

        MIO::initialise(config);

        return;
    }


    eckit::Tokenizer parse(":");

    StringList sinks;
    parse(::getenv("MULTIO_SINKS") ? ::getenv("MULTIO_SINKS") : "fdb4", sinks);

    ASSERT(sinks.size());

    std::ostringstream oss;

    oss << "{ \"sinks\" : [";

    const char *sep = "";
    for (StringList::iterator i = sinks.begin(); i != sinks.end(); ++i) {
        oss << sep << "{ \"type\" : \"" << *i << "\" }";
        sep = ",";
    }
    oss << "] }";

    std::cout << "MultIO initialising with $MULTIO_SINKS " << oss.str() << std::endl;

    std::istringstream iss(oss.str());

    eckit::JSONConfiguration config(iss);

    MIO::initialise(config);

}

/**********************************************************************************************************************/

#define MULTIO_TRACE

static bool traceme() {
    static char* trace = ::getenv("MULTIO_TRACE");
    if(trace == 0) {
        return false;
    }
    return true;
}

#ifdef  MULTIO_TRACE
#define MULTIO_TRACE_FUNC()       if(traceme()) { fprintf(stdout,"MULTIO %s : %s()\n",MULTIO_VERSION,__FUNCTION__); }
#define MULTIO_TRACE_FUNC1(p1)    if(traceme()) { fprintf(stdout,"MULTIO %s : %s(%s)\n",MULTIO_VERSION,__FUNCTION__,p1); }
#define MULTIO_TRACE_FUNC2(p1,p2) if(traceme()) { fprintf(stdout,"MULTIO %s : %s(%s,%s)\n",MULTIO_VERSION,__FUNCTION__,p1,p2); }
#else
#define MULTIO_TRACE_FUNC()
#define MULTIO_TRACE_FUNC1(p1)
#define MULTIO_TRACE_FUNC2(p1,p2)
#endif

/**********************************************************************************************************************/

extern "C" {

    fortint iinitfdb_() {

        try {

            MULTIO_TRACE_FUNC();

            pthread_once(&once, init);
            eckit::AutoLock<eckit::Mutex> lock(local_mutex);

            MIO::instance().iinitfdb();

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    fortint iinitfdb_vpp_(const char *name, int name_len) {

        try {

            std::string sname(name, name + name_len);

            MULTIO_TRACE_FUNC1(sname.c_str());

            pthread_once(&once, init);
            eckit::AutoLock<eckit::Mutex> lock(local_mutex);

            MIO::instance().iinitfdb();

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    fortint iopenfdb_(const char *name, fortint *addr, const char *mode, int name_len, int mode_len) {

        try {

            std::string sname(name, name + name_len);
            std::string smode(mode, mode + mode_len);

            MULTIO_TRACE_FUNC2(sname.c_str(), smode.c_str());

            pthread_once(&once, init);
            eckit::AutoLock<eckit::Mutex> lock(local_mutex);

            ASSERT(addr);

            int fdbaddr = 0;
            MIO::instance().iopenfdb(sname, fdbaddr, smode);
            *addr = fdbaddr;

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    fortint iclosefdb_(fortint *addr) {

        try {

            MULTIO_TRACE_FUNC();

            pthread_once(&once, init);
            eckit::AutoLock<eckit::Mutex> lock(local_mutex);

            ASSERT(addr);

            MIO::instance().iclosefdb(*addr);

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    fortint iflushfdb_(const fortint *addr) {

        try {

            MULTIO_TRACE_FUNC();

            ASSERT(addr);

            MIO::instance().iflushfdb(*addr);

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    fortint iwritefdb_(const fortint *addr, const void *data, const fortint *words) {

        try {

            MULTIO_TRACE_FUNC();

            ASSERT(addr);

            size_t len( (*words)*sizeof(fortint) );

            eckit::DataBlobPtr blob ( new metkit::grib::GribDataBlob(data, len) );

            MIO::instance().iwritefdb(*addr, blob);

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    fortint iset_fdb_root_(const fortint *addr, const char *name, int name_len) {

        try {

            std::string sname(name, name + name_len);

            MULTIO_TRACE_FUNC1(sname.c_str());

            pthread_once(&once, init);
            eckit::AutoLock<eckit::Mutex> lock(local_mutex);

            ASSERT(addr);

            MIO::instance().iset_fdb_root(*addr, sname);

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    fortint isetvalfdb_(const fortint *addr, const char *name, const char *value, int name_len, int value_len) {

        try {

            std::string sname(name, name + name_len);
            std::string svalue(value, value + value_len);

            MULTIO_TRACE_FUNC2(sname.c_str(), svalue.c_str());

            ASSERT(addr);

            MIO::instance().isetvalfdb(*addr, sname, svalue);

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    int isetcommfdb_(const fortint *comm) {

        try {

            MULTIO_TRACE_FUNC();

            pthread_once(&once, init);
            eckit::AutoLock<eckit::Mutex> lock(local_mutex);

            MIO::instance().isetcommfdb(*comm);

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    int isetrankfdb_(const fortint *addr, const fortint *rank) {

        try {

            MULTIO_TRACE_FUNC();

            ASSERT(addr);

            MIO::instance().isetrankfdb(*addr, *rank);

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    int isetfieldcountfdb_(const fortint *addr, const fortint *all_ranks, const fortint *this_rank) {

        try {

            MULTIO_TRACE_FUNC();

            ASSERT(addr);

            MIO::instance().isetfieldcountfdb(*addr, *all_ranks, *this_rank);

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

    fortint ireadfdb_(const fortint *addr, void *data, fortint *words) {

        try {

            MULTIO_TRACE_FUNC();

            NOTIMP;

        } catch (std::exception &e) {
            eckit::Log::error() << "FDB MultIO wrapper: " << e.what() << std::endl;
            return -2;
        }
        return 0;
    }

} // extern C
