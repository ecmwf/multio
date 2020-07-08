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
#include "multio/ifsio/ifsio.h"
#include "multio/ifsio/ifsio_internals.h"
#include "multio/ifsio/EncodeBitsPerValue.h"

#include "multio/sink/MultIO.h"

#include "metkit/grib/GribDataBlob.h"

using namespace eckit;
using namespace multio;

//----------------------------------------------------------------------------------------------------------------------

class MIO {
public:

    static MIO& instance() {
        static MIO mio;
        return mio;
    }


    MultIO& mio() {
        ASSERT(ptr_);
        return *ptr_;
    }

    void log(bool log) { log_ = log; }
    void dirty(bool dirty) { dirty_ = dirty; }

    void report() {
        if(log_ && ptr_ && !::getenv("MULTIO_NO_REPORT")) {
            ptr_->report(std::cout);
        }
    }

    void lock()   { mutex_.lock(); }
    void unlock() { mutex_.unlock(); }

    int encodeBitsPerValue(int paramid, const std::string& levtype, double min, double max) {
      ASSERT(bpv_);
      return bpv_->getBitsPerValue(paramid, levtype, min, max);
    }

private:

    void init(Configuration& config) {
      ptr_.reset(new MultIO(config));
      bpv_.reset(new EncodeBitsPerValue(config));
    }

    MIO() : log_(false), dirty_(false) {
        static const char *argv[2] = {"ifsio", nullptr};

        eckit::Main::initialise(1, const_cast<char**>(argv));

        if (::getenv("MULTIO_CONFIG")) {
            std::string cfg(::getenv("MULTIO_CONFIG"));
            std::cout << "MultIO initialising with config " << cfg << std::endl;
            eckit::YAMLConfiguration config(cfg);
            init(config);
            return;
        }

        if (::getenv("MULTIO_CONFIG_FILE")) {
            PathName path(::getenv("MULTIO_CONFIG_FILE"));
            std::cout << "MultIO initialising with file " << path << std::endl;
            eckit::YAMLConfiguration config(path);
            init(config);
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
            oss << "}";
            sep = ",";
        }
        oss << "] }";

        std::cout << "MultIO initialising with $MULTIO_SINKS " << oss.str() << std::endl;

        std::istringstream iss(oss.str());
        eckit::YAMLConfiguration config(iss);
        init(config);
    }

    ~MIO() {
        if(dirty_) {
            static char* abort_on_error = ::getenv("MULTIO_ABORT_ON_ERROR");
            if(abort_on_error) {
                std::cout << "ERROR - MultIO finished without a final call to imultio_flush" << std::endl;
                std::cerr << "ERROR - MultIO finished without a final call to imultio_flush" << std::endl;
                eckit::LibEcKit::instance().abort();
            }
            else
                std::cout << "WARNING - MultIO finished without a final call to imultio_flush" << std::endl;
        }
    }

    std::unique_ptr<MultIO> ptr_;
    std::unique_ptr<EncodeBitsPerValue> bpv_;
    eckit::Mutex mutex_;
    bool log_;
    bool dirty_;
};

//----------------------------------------------------------------------------------------------------------------------


extern "C" {

    fortint imultio_flush_() {
        try {
            eckit::AutoLock<MIO> lock(MIO::instance());

            MULTIO_TRACE_FUNC();
            MIO::instance().mio().flush();
            MIO::instance().log(true);
            MIO::instance().dirty(false);
        } catch (std::exception &e) {
            return ifsio_handle_error(e);
        }
        return 0;
    }

    fortint imultio_notify_step_(const fortint * step) {
        try {
            eckit::AutoLock<MIO> lock(MIO::instance());

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
            eckit::AutoLock<MIO> lock(MIO::instance());

            MULTIO_TRACE_FUNC();
            ASSERT(data);
            int ilen = (*words)*sizeof(fortint);
            ASSERT(ilen > 0);
            size_t len(ilen);

            eckit::DataBlobPtr blob (new metkit::grib::GribDataBlob(data, len));

            MIO::instance().mio().write(blob);
            MIO::instance().log(true);
            MIO::instance().dirty(true);
        } catch (std::exception &e) {
            return ifsio_handle_error(e);
        }
        return 0;
    }

    fortint imultio_encode_bitspervalue_(fortint *bitspervalue, const fortint *paramid, const char* levtype,  const double *min,  const double *max, int levtype_len) {
        try {
            std::string slevtype(levtype, levtype + levtype_len);
            eckit::AutoLock<MIO> lock(MIO::instance());
            *bitspervalue = MIO::instance().encodeBitsPerValue(*paramid, slevtype, *min, *max);
        } catch (std::exception &e) {
            return ifsio_handle_error(e);
        }
        return 0;
    }

} // extern C
