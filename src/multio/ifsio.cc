/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "eckit/config/LibEcKit.h"
#include "eckit/types/Types.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/utils/Tokenizer.h"
#include "eckit/runtime/Main.h"
#include "eckit/thread/AutoLock.h"

#include "multio/multio_version.h"
#include "multio/ifsio.h"

#include "multio/MultIO.h"

#include "metkit/grib/GribDataBlob.h"

typedef int32_t fortint;

using namespace eckit;
using namespace multio;

static eckit::Mutex *local_mutex = 0;

static pthread_once_t once = PTHREAD_ONCE_INIT;

//----------------------------------------------------------------------------------------------------------------------

class MIO {
public:

    static void initialise(const eckit::YAMLConfiguration& config) {
        MIO& mio = instance();
        if(mio.ptr_) return;
        mio.ptr_.reset(new MultIO(config));
    }

    static MIO& instance() {
        static MIO mio;
        return mio;
    }


    MultIO& mio() {
        ASSERT(ptr_);
        return *ptr_;
    }

    void log(bool log) { log_ = log; }

    void report() {
        if(log_ && ptr_ && !::getenv("MULTIO_NO_REPORT")) {
            ptr_->report(std::cout);
        }
    }

private:

    MIO() : log_(false) {}

    ~MIO() {}

    std::unique_ptr<MultIO> ptr_;

    bool log_;
};

static void init() {

    local_mutex = new eckit::Mutex();

    static const char *argv[2] = {"ifsio", 0};

    eckit::Main::initialise(1, const_cast<char**>(argv));

    if (::getenv("MULTIO_CONFIG_FILE")) {

        PathName path(::getenv("MULTIO_CONFIG_FILE"));

        std::cout << "MultIO initialising with file " << path << std::endl;

        eckit::YAMLConfiguration config(path);

        MIO::initialise(config);

        return;
    }


    eckit::Tokenizer parse(":");

    StringList sinks;
    parse(::getenv("MULTIO_SINKS") ? ::getenv("MULTIO_SINKS") : "fdb5", sinks);

    ASSERT(sinks.size());

    std::ostringstream oss;

    oss << "{ \"sinks\" : [";

    const char *sep = "";
    for (StringList::iterator i = sinks.begin(); i != sinks.end(); ++i) {
        oss << sep << "{ \"type\" : \"" << *i << "\"";

        // By default, when using the legacy interface, configure the fdb5 to use sub tocs
        if (*i == "fdb5")
            oss << ", \"useSubToc\": true";

        oss << "}";
        sep = ",";
    }
    oss << "] }";

    std::cout << "MultIO initialising with $MULTIO_SINKS " << oss.str() << std::endl;

    std::istringstream iss(oss.str());

    eckit::YAMLConfiguration config(iss);

    MIO::initialise(config);

}

//----------------------------------------------------------------------------------------------------------------------

#define MULTIO_TRACE

static bool traceme() {
    static char* trace = ::getenv("MULTIO_TRACE");
    if(trace == 0) {
        return false;
    }
    return true;
}

#ifdef  MULTIO_TRACE
#define MULTIO_TRACE_FUNC()       if(traceme()) { fprintf(stdout,"MULTIO %s : %s()\n",MULTIO_VERSION,__func__); }
#define MULTIO_TRACE_FUNC1(p1)    if(traceme()) { fprintf(stdout,"MULTIO %s : %s(%s)\n",MULTIO_VERSION,__func__,p1); }
#define MULTIO_TRACE_FUNC2(p1,p2) if(traceme()) { fprintf(stdout,"MULTIO %s : %s(%s,%s)\n",MULTIO_VERSION,__func__,p1,p2); }
#else
#define MULTIO_TRACE_FUNC()
#define MULTIO_TRACE_FUNC1(p1)
#define MULTIO_TRACE_FUNC2(p1,p2)
#endif

//----------------------------------------------------------------------------------------------------------------------

static int ifsio_handle_error(std::exception& e) {

    std::cout << "FDB MultIO wrapper: " << e.what() << std::endl << std::flush;
    std::cerr << "FDB MultIO wrapper: " << e.what() << std::endl << std::flush;

    static char* abort_on_error = ::getenv("MULTIO_ABORT_ON_ERROR");
    if(abort_on_error) {
        std::cout << "FDB MultIO wrapper: MULTIO_ABORT_ON_ERROR is SET -- aborting ... " << std::endl << std::flush;
        std::cerr << "FDB MultIO wrapper: MULTIO_ABORT_ON_ERROR is SET -- aborting ... " << std::endl << std::flush;

        eckit::LibEcKit::instance().abort();
    }

    return -2;
}


extern "C" {

    fortint imultio_flush_() {
        try {
            MULTIO_TRACE_FUNC();
            MIO::instance().mio().flush();
            MIO::instance().log(true);
        } catch (std::exception &e) {
            return ifsio_handle_error(e);
        }
        return 0;
    }

    fortint imultio_notify_step_(const fortint * step) {
        try {
            MULTIO_TRACE_FUNC();
            ASSERT(step);
            eckit::StringDict metadata;
            metadata["step"] = eckit::Translator<fortint,std::string>()(*step);
            MIO::instance().mio().trigger(metadata);
        } catch (std::exception &e) {
            return ifsio_handle_error(e);
        }
        return 0;
    }

    fortint imultio_write_(const void *data, const fortint *words) {
        try {
            MULTIO_TRACE_FUNC();
            ASSERT(data);
            int ilen = (*words)*sizeof(fortint);
            ASSERT(ilen > 0);
            size_t len(ilen);

            eckit::DataBlobPtr blob (new metkit::grib::GribDataBlob(data, len));

            MIO::instance().mio().write(blob);
            MIO::instance().log(true);

        } catch (std::exception &e) {
            return ifsio_handle_error(e);
        }
        return 0;
    }

} // extern C
