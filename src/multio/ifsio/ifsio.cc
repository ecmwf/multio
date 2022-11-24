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
#include "eckit/config/Resource.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/message/Message.h"
#include "eckit/runtime/Main.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/types/Types.h"
#include "eckit/utils/Tokenizer.h"

#include "multio/ifsio/EncodeBitsPerValue.h"
#include "multio/ifsio/ifsio.h"
#include "multio/ifsio/ifsio_internals.h"
#include "multio/multio_version.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/FailureHandling.h"

#include "multio/action/Plan.h"
#include "multio/action/Sink.h"
#include "multio/sink/MultIO.h"

#include "metkit/codes/CodesContent.h"

using namespace eckit;
using namespace metkit;
using namespace multio;
using namespace multio::util;
using namespace multio::message;

//----------------------------------------------------------------------------------------------------------------------

class MIO : public util::FailureAware<ComponentTag::Client> {
public:
    static MIO& instance() {
        static MIO mio;
        return mio;
    }


    void log(bool log) { log_ = log; }
    void dirty(bool dirty) { dirty_ = dirty; }


    void lock() { mutex_.lock(); }
    void unlock() { mutex_.unlock(); }

    int encodeBitsPerValue(int paramid, const std::string& levtype, double min, double max) {
        ASSERT(bpv_);
        return bpv_->getBitsPerValue(paramid, levtype, min, max);
    }

    void dispatch(const multio::message::Message& msg) {
        withFailureHandling(
            [&]() {
                for (const auto& plan : plans_) {
                    plan->process(msg);
                }
            },
            [msg]() {
                std::ostringstream oss;
                oss << "IFSIO dispatching message: " << msg;
                return oss.str();
            });
    };

    util::FailureHandlerResponse handleFailure(util::OnClientError, const util::FailureContext& c,
                                               util::DefaultFailureState&) const override {
        // Last cascading instance, print nested contexts
        print(eckit::Log::error(), c);
        return util::FailureHandlerResponse::Rethrow;
    };


private:
    ConfigurationContext configureFromSinks(const ConfigurationContext& confCtx) {
        std::vector<eckit::LocalConfiguration> actions;
        actions.push_back(confCtx.config());
        actions[0].set("type", "sink");

        std::vector<eckit::LocalConfiguration> plans;
        plans.push_back(eckit::LocalConfiguration{});
        plans[0].set("actions", actions);

        eckit::LocalConfiguration cfg;
        cfg.set("plans", plans);

        return confCtx.recast(cfg);
    }

    MIO(const ConfigurationContext& confCtx) : FailureAware(confCtx), log_(false), dirty_(false) {
        for (auto&& cfg : confCtx.subContexts("plans", ComponentTag::Plan)) {
            plans_.emplace_back(new action::Plan(std::move(cfg)));
        }
        bpv_.reset(new EncodeBitsPerValue(confCtx.config()));
    }

    MIO() : MIO(configureFromEnv().setComponentTag(multio::util::ComponentTag::Client)) {}

    ConfigurationContext configureFromEnv() {
        static const char* argv[2] = {"ifsio", nullptr};

        eckit::Main::initialise(1, const_cast<char**>(argv));

        if (::getenv("MULTIO_PLANS")) {
            std::string cfg(::getenv("MULTIO_PLANS"));
            std::cout << "MultIO initialising with plans " << cfg << std::endl;
            return ConfigurationContext(eckit::LocalConfiguration(eckit::YAMLConfiguration(cfg)), cfg, cfg);
        }

        if (::getenv("MULTIO_PLANS_FILE")) {
            PathName path(::getenv("MULTIO_PLANS_FILE"));
            std::cout << "MultIO initialising with plans file " << path << std::endl;
            return ConfigurationContext(eckit::LocalConfiguration(eckit::YAMLConfiguration(path)), path.dirName(), path);
        }

        if (::getenv("MULTIO_CONFIG")) {
            std::string cfg(::getenv("MULTIO_CONFIG"));
            std::cout << "MultIO initialising with config " << cfg << std::endl;
            return configureFromSinks(
                ConfigurationContext(eckit::LocalConfiguration(eckit::YAMLConfiguration(cfg)), cfg, cfg));
        }

        if (::getenv("MULTIO_CONFIG_FILE")) {
            PathName path(::getenv("MULTIO_CONFIG_FILE"));
            std::cout << "MultIO initialising with config file " << path << std::endl;
            return configureFromSinks(
                ConfigurationContext(eckit::LocalConfiguration(eckit::YAMLConfiguration(path)), path.dirName(), path));
        }

        eckit::Tokenizer parse(":");

        StringList sinks;
        parse(::getenv("MULTIO_SINKS") ? ::getenv("MULTIO_SINKS") : "fdb5", sinks);

        ASSERT(sinks.size());

        std::ostringstream oss;

        oss << "{ \"sinks\" : [";

        const char* sep = "";
        for (StringList::iterator i = sinks.begin(); i != sinks.end(); ++i) {
            oss << sep << "{ \"type\" : \"" << *i << "\"";
            oss << "}";
            sep = ",";
        }
        oss << "] }";

        std::cout << "MultIO initialising with $MULTIO_SINKS " << oss.str() << std::endl;

        std::istringstream iss(oss.str());
        return configureFromSinks(
            ConfigurationContext(eckit::LocalConfiguration(eckit::YAMLConfiguration(iss)), "", ""));
    }

    ~MIO() {
        if (dirty_) {
            static char* abort_on_error = ::getenv("MULTIO_ABORT_ON_ERROR");
            if (abort_on_error) {
                std::cout << "ERROR - MultIO finished without a final call to imultio_flush" << std::endl;
                std::cerr << "ERROR - MultIO finished without a final call to imultio_flush" << std::endl;
                eckit::LibEcKit::instance().abort();
            }
            else
                std::cout << "WARNING - MultIO finished without a final call to imultio_flush" << std::endl;
        }
    }

    std::vector<std::unique_ptr<action::Plan>> plans_;
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

        multio::message::Metadata metadata;
        multio::message::Message message{
            multio::message::Message::Header{Message::Tag::StepComplete, Peer{}, Peer{}, std::move(metadata)},
            eckit::Buffer{0}};
        MIO::instance().dispatch(message);

        MIO::instance().log(true);
        MIO::instance().dirty(false);
    }
    catch (std::exception& e) {
        return ifsio_handle_error(e);
    }
    return 0;
}

fortint imultio_notify_step_(const fortint* step) {
    try {
        eckit::AutoLock<MIO> lock(MIO::instance());

        MULTIO_TRACE_FUNC();
        ASSERT(step);

        multio::message::Metadata metadata;
        metadata.set("trigger", "step");
        metadata.set("name", eckit::Translator<fortint, std::string>()(*step));
        multio::message::Message message{
            multio::message::Message::Header{Message::Tag::StepNotification, Peer{}, Peer{}, std::move(metadata)},
            eckit::Buffer{0}};
        MIO::instance().dispatch(message);
    }
    catch (std::exception& e) {
        return ifsio_handle_error(e);
    }
    return 0;
}

fortint imultio_write_(const void* data, const fortint* words) {
    try {
        eckit::AutoLock<MIO> lock(MIO::instance());

        MULTIO_TRACE_FUNC();
        ASSERT(data);
        int ilen = (*words) * sizeof(fortint);
        ASSERT(ilen > 0);
        size_t len(ilen);

        eckit::Buffer payload{reinterpret_cast<const char*>(data), len};

        multio::message::Metadata metadata;
        multio::message::Message message{
            multio::message::Message::Header{Message::Tag::Grib, Peer{}, Peer{}, std::move(metadata)},
            std::move(payload)};
        MIO::instance().dispatch(message);

        MIO::instance().log(true);
        MIO::instance().dirty(true);
    }
    catch (std::exception& e) {
        return ifsio_handle_error(e);
    }
    return 0;
}


fortint imultio_write_raw_(const void* configuration, const void* data, const fortint* words) {
    try {
        eckit::AutoLock<MIO> lock(MIO::instance());

        MULTIO_TRACE_FUNC();
        ASSERT(configuration);
        const eckit::Configuration* conf = reinterpret_cast<const eckit::Configuration*>(configuration);
        
        ASSERT(data);
        int ilen = (*words) * sizeof(fortint);
        ASSERT(ilen > 0);
        size_t len(ilen);

        eckit::Buffer payload{reinterpret_cast<const char*>(data), len};

        multio::message::Metadata metadata{*conf};
        multio::message::Message message{
            multio::message::Message::Header{Message::Tag::Field, Peer{}, Peer{}, std::move(metadata)},
            std::move(payload)};
        MIO::instance().dispatch(message);

        MIO::instance().log(true);
        MIO::instance().dirty(true);
    }
    catch (std::exception& e) {
        return ifsio_handle_error(e);
    }
    return 0;
}

fortint imultio_encode_bitspervalue_(fortint* bitspervalue, const fortint* paramid, const char* levtype,
                                     const double* min, const double* max, int levtype_len) {
    try {
        std::string slevtype(levtype, levtype + levtype_len);
        eckit::AutoLock<MIO> lock(MIO::instance());
        *bitspervalue = MIO::instance().encodeBitsPerValue(*paramid, slevtype, *min, *max);
    }
    catch (std::exception& e) {
        return ifsio_handle_error(e);
    }
    return 0;
}

}  // extern C
