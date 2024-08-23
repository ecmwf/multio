
#include "multio_c.h"
#include "multio_c_cpp_utils.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/runtime/Main.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/config/ConfigurationPath.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/domain/MaskCompression.h"
#include "multio/message/Metadata.h"
#include "multio/multio_version.h"
#include "multio/server/MultioClient.h"
#include "multio/server/MultioServer.h"
#include "multio/util/FailureHandling.h"

#include <functional>
#include <optional>

using multio::message::Message;
using multio::message::Metadata;
using multio::message::Peer;

using multio::config::ComponentConfiguration;
using multio::config::configuration_file;
using multio::config::configuration_file_name;
using multio::config::configuration_path_name;
using multio::config::MPIInitInfo;
using multio::config::MultioConfiguration;
using multio::util::FailureAwareException;

extern "C" {

struct multio_failure_info_t {
    std::string lastErrorString{""};
};

struct multio_failure_context_t {
    multio_failure_handler_t handler{nullptr};
    void* usercontext{nullptr};
    multio_failure_info_t info;
};

static multio_failure_info_t g_failure_info;

const char* multio_error_string_info(int err, multio_failure_info_t* info) {
    switch (err) {
        case MULTIO_SUCCESS:
            return "Success";
        case MULTIO_ERROR_ECKIT_EXCEPTION:
        case MULTIO_ERROR_GENERAL_EXCEPTION:
        case MULTIO_ERROR_UNKNOWN_EXCEPTION:
            return info->lastErrorString.c_str();
        default:
            return "<unknown>";
    };
}

const char* multio_error_string(int err) {
    return multio_error_string_info(err, &g_failure_info);
}

struct multio_configuration_t : public MultioConfiguration {
    multio_configuration_t(const eckit::PathName& fileName) : MultioConfiguration{fileName} {}

    multio_configuration_t() : MultioConfiguration{} {}

    std::unique_ptr<multio_failure_context_t> failureContext;
};

struct multio_handle_t : public multio::server::MultioClient {
    using multio::server::MultioClient::MultioClient;
    multio_handle_t() : MultioClient{} {}

    multio_handle_t(MultioConfiguration&& multioConf) : MultioClient{std::move(multioConf)} {}

    std::unique_ptr<multio_failure_context_t> failureContext;
};

struct multio_metadata_t : public multio::message::Metadata {
    using multio::message::Metadata::Metadata;
    multio_handle_t* mio;
};


}  // extern "C"


namespace {
// Template magic to provide a consistent error-handling approach

int innerWrapFn(std::function<void()> f) {
    f();
    return MULTIO_SUCCESS;
}

MultioErrorValues errorValue(const FailureAwareException& e) {
    return MULTIO_ERROR_ECKIT_EXCEPTION;
}
MultioErrorValues errorValue(const eckit::Exception& e) {
    return MULTIO_ERROR_ECKIT_EXCEPTION;
}
MultioErrorValues errorValue(const std::exception& e) {
    return MULTIO_ERROR_GENERAL_EXCEPTION;
}
template <class E>
MultioErrorValues errorValue(const E& e) {
    return MULTIO_ERROR_UNKNOWN_EXCEPTION;
}

template <class E>
MultioErrorValues getNestedErrorValue(const E& e) {
    try {
        std::rethrow_if_nested(e);
        // Return passed error value if not nested
        return errorValue(e);
    }
    catch (const FailureAwareException& nestedException) {
        return getNestedErrorValue(nestedException);
    }
    catch (const eckit::Exception& nestedException) {
        return getNestedErrorValue(nestedException);
    }
    catch (const std::exception& nestedException) {
        return getNestedErrorValue(nestedException);
    }
    catch (...) {
        return MULTIO_ERROR_UNKNOWN_EXCEPTION;
    }
}

void callFailureHandler(multio_failure_context_t* fctx, int err) {
    fctx->handler(fctx->usercontext, err, &fctx->info);
}

template <typename FN>
int wrapApiFunction(FN f, multio_failure_context_t* fh = nullptr) {
    try {
        return innerWrapFn(f);
    }
    catch (FailureAwareException& e) {
        std::ostringstream oss;
        oss << "Caught a nested exception on C-C++ API boundary: ";
        oss << e;

        MultioErrorValues error = getNestedErrorValue(e);
        g_failure_info.lastErrorString = oss.str();
        if (fh && fh->handler) {
            fh->info.lastErrorString = oss.str();
            callFailureHandler(fh, error);
            return error;
        }

        // Print to cerr and cout to make sure the user knows his problem
        std::cerr << oss.str() << std::endl;
        std::cout << oss.str() << std::endl;
        return error;
    }
    catch (eckit::Exception& e) {
        int error = MULTIO_ERROR_ECKIT_EXCEPTION;
        std::ostringstream oss;
        oss << "Caught eckit exception on C-C++ API boundary: " << e.what();

        g_failure_info.lastErrorString = oss.str();
        if (fh && fh->handler) {
            fh->info.lastErrorString = oss.str();
            callFailureHandler(fh, error);
            return error;
        }

        // Print to cerr and cout to make sure the user knows his problem
        std::cerr << oss.str() << std::endl;
        std::cout << oss.str() << std::endl;
        return error;
    }
    catch (std::exception& e) {
        int error = MULTIO_ERROR_GENERAL_EXCEPTION;
        std::ostringstream oss;
        oss << "Caught exception on C-C++ API boundary: " << e.what();

        g_failure_info.lastErrorString = oss.str();
        if (fh && fh->handler) {
            fh->info.lastErrorString = oss.str();
            callFailureHandler(fh, error);
            return error;
        }

        // Print to cerr and cout to make sure the user knows his problem
        std::cerr << oss.str() << std::endl;
        std::cout << oss.str() << std::endl;
        return error;
    }
    catch (...) {
        int error = MULTIO_ERROR_UNKNOWN_EXCEPTION;
        std::string errStr = "Caugth unkown exception on C-C++ API boundary";

        g_failure_info.lastErrorString = errStr;
        if (fh && fh->handler) {
            fh->info.lastErrorString = errStr;
            callFailureHandler(fh, error);
            return error;
        }

        // Print to cerr and cout to make sure the user knows his problem
        std::cerr << errStr << std::endl;
        std::cout << errStr << std::endl;
        return error;
    }

    ASSERT(false);
}

template <typename FN>
int wrapApiFunction(FN&& f, multio_configuration_t* cc) {
    return wrapApiFunction(std::forward<FN>(f), cc ? cc->failureContext.get() : nullptr);
}

template <typename FN>
int wrapApiFunction(FN&& f, multio_handle_t* mio) {
    return wrapApiFunction(std::forward<FN>(f), mio ? mio->failureContext.get() : nullptr);
}

template <typename FN>
int wrapApiFunction(FN&& f, multio_metadata_t* md) {
    return wrapApiFunction(std::forward<FN>(f), (md && md->mio) ? md->mio->failureContext.get() : nullptr);
}


}  // namespace

extern "C" {

int multio_initialise() {
    return wrapApiFunction([] {
        static bool initialised = false;

        if (initialised) {
            eckit::Log::warning() << "Initialising MultIO library twice" << std::endl;
        }

        if (!initialised) {
            const char* argv[2] = {"multio-api", 0};
            eckit::Main::initialise(1, const_cast<char**>(argv));
            initialised = true;
        }
    });
}

int multio_version(const char** version) {
    return wrapApiFunction([version]() { (*version) = multio_version_str(); });
}

int multio_vcs_version(const char** sha1) {
    return wrapApiFunction([sha1]() { (*sha1) = multio_git_sha1(); });
}

int multio_config_set_failure_handler(multio_configuration_t* cc, multio_failure_handler_t handler, void* usercontext) {
    return wrapApiFunction(
        [handler, usercontext, cc] {
            if (!cc->failureContext) {
                cc->failureContext = std::make_unique<multio_failure_context_t>();
            }
            auto f = cc->failureContext.get();
            f->handler = handler;
            f->usercontext = usercontext;
            eckit::Log::info() << "MultIO setting failure handler callable" << std::endl;
        },
        cc);
}

int multio_handle_set_failure_handler(multio_handle_t* mio, multio_failure_handler_t handler, void* usercontext) {
    return wrapApiFunction(
        [handler, usercontext, mio] {
            if (!mio->failureContext) {
                mio->failureContext = std::make_unique<multio_failure_context_t>();
            }
            auto f = mio->failureContext.get();
            f->handler = handler;
            f->usercontext = usercontext;
            eckit::Log::info() << "MultIO setting failure handler callable" << std::endl;
        },
        mio);
}

int multio_new_configuration(multio_configuration_t** cc) {
    return wrapApiFunction([cc]() { (*cc) = new multio_configuration_t{}; });
};

int multio_new_configuration_from_filename(multio_configuration_t** cc, const char* conf_file_name) {
    return wrapApiFunction([cc, conf_file_name]() {
        ASSERT(conf_file_name);
        (*cc) = new multio_configuration_t{eckit::PathName{conf_file_name}};
    });
};

int multio_delete_configuration(multio_configuration_t* cc) {
    // delete without failurehandler. Configuration is assumed to have be invalid after movement
    return wrapApiFunction([cc]() {
        ASSERT(cc);
        delete cc;
    });
};

int multio_conf_set_path(multio_configuration_t* cc, const char* configuration_path) {
    return wrapApiFunction(
        [cc, configuration_path]() {
            ASSERT(cc);
            if (configuration_path != nullptr) {
                cc->setConfigDir(eckit::PathName(configuration_path));
            }
        },
        cc);
};

int multio_conf_mpi_allow_world_default_comm(multio_configuration_t* cc, bool allow) {
    return wrapApiFunction(
        [cc, allow]() {
            ASSERT(cc);
            if (!cc->getMPIInitInfo()) {
                cc->setMPIInitInfo(std::optional<MPIInitInfo>{MPIInitInfo{}});
            }
            cc->getMPIInitInfo().value().allowWorldAsDefault = allow;
        },
        cc);
};

int multio_conf_mpi_parent_comm(multio_configuration_t* cc, int parent_comm) {
    return wrapApiFunction(
        [cc, parent_comm]() {
            ASSERT(cc);
            if (!cc->getMPIInitInfo()) {
                cc->setMPIInitInfo(std::optional<MPIInitInfo>{MPIInitInfo{}});
            }
            cc->getMPIInitInfo().value().parentComm = parent_comm;
        },
        cc);
};

int multio_conf_mpi_return_client_comm(multio_configuration_t* cc, int* return_client_comm) {
    return wrapApiFunction(
        [cc, return_client_comm]() {
            ASSERT(cc);
            if (!cc->getMPIInitInfo()) {
                cc->setMPIInitInfo(std::optional<MPIInitInfo>{MPIInitInfo{}});
            }
            cc->getMPIInitInfo().value().returnClientComm = return_client_comm;
        },
        cc);
};

int multio_conf_mpi_return_server_comm(multio_configuration_t* cc, int* return_server_comm) {
    return wrapApiFunction(
        [cc, return_server_comm]() {
            ASSERT(cc);
            if (!cc->getMPIInitInfo()) {
                cc->setMPIInitInfo(std::optional<MPIInitInfo>{MPIInitInfo{}});
            }
            cc->getMPIInitInfo().value().returnServerComm = return_server_comm;
        },
        cc);
};

int multio_new_handle(multio_handle_t** mio, multio_configuration_t* cc) {
    // Failurehandler and its string location is preserved through shared_ptr...
    return wrapApiFunction(
        [mio, cc]() {
            ASSERT(cc);
            (*mio) = new multio_handle_t{std::move(*cc)};

            // Finally move failureContext (should not throw...)
            (**mio).failureContext = std::move(cc->failureContext);
        },
        cc);
}

int multio_new_handle_default(multio_handle_t** mio) {
    // Failurehandler and its string location is preserved through shared_ptr...
    return wrapApiFunction([mio]() { (*mio) = new multio_handle_t{}; });
}

int multio_delete_handle(multio_handle_t* mio) {
    return wrapApiFunction([mio]() {
        ASSERT(mio);
        delete mio;
    });
}

int multio_start_server(multio_configuration_t* cc) {
    // Failurehandler and its string location is preserved through shared_ptr...
    return wrapApiFunction(
        [cc]() {
            ASSERT(cc);
            multio::server::MultioServer{std::move(*cc)};
        },
        cc);
}

int multio_open_connections(multio_handle_t* mio) {
    return wrapApiFunction(
        [mio]() {
            ASSERT(mio);

            mio->openConnections();
        },
        mio);
}

int multio_close_connections(multio_handle_t* mio) {
    return wrapApiFunction(
        [mio]() {
            ASSERT(mio);

            mio->closeConnections();
        },
        mio);
}

int multio_flush(multio_handle_t* mio, multio_metadata_t* md) {
    return wrapApiFunction(
        [mio, md]() {
            ASSERT(mio);
            ASSERT(md);

            mio->dispatch(*md, eckit::Buffer{0}, Message::Tag::Flush);
        },
        mio);
}


int multio_notify(multio_handle_t* mio, multio_metadata_t* md) {
    return wrapApiFunction(
        [mio, md]() {
            ASSERT(mio);
            ASSERT(md);

            mio->dispatch(*md, eckit::Buffer{0}, Message::Tag::Notification);
        },
        mio);
}


int multio_write_domain(multio_handle_t* mio, multio_metadata_t* md, int* data, int size) {
    return wrapApiFunction(
        [mio, md, data, size]() {
            ASSERT(mio);
            ASSERT(md);

            eckit::Buffer domain_def{reinterpret_cast<const char*>(data), size * sizeof(int)};
            mio->dispatch(*md, std::move(domain_def), Message::Tag::Domain);
        },
        mio);
}

int multio_write_mask_float(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size) {
    return wrapApiFunction(
        [mio, md, data, size]() {
            ASSERT(mio);
            ASSERT(md);

            eckit::Buffer mask_vals = multio::domain::encodeMask(data, size);
            mio->dispatch(*md, std::move(mask_vals), Message::Tag::Mask);
        },
        mio);
}

int multio_write_mask_double(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return wrapApiFunction(
        [mio, md, data, size]() {
            ASSERT(mio);
            ASSERT(md);

            eckit::Buffer mask_vals = multio::domain::encodeMask(data, size);
            mio->dispatch(*md, std::move(mask_vals), Message::Tag::Mask);
        },
        mio);
}

int multio_write_field_float(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size) {
    return wrapApiFunction(
        [mio, md, data, size]() {
            ASSERT(mio);
            ASSERT(md);

            md->set("precision", "single");

            eckit::Buffer field_vals{reinterpret_cast<const char*>(data), size * sizeof(float)};

            mio->dispatch(*md, std::move(field_vals), Message::Tag::Field);
        },
        mio);
}

int multio_write_field_double(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return wrapApiFunction(
        [mio, md, data, size]() {
            ASSERT(mio);
            ASSERT(md);

            md->set("precision", "double");

            eckit::Buffer field_vals{reinterpret_cast<const char*>(data), size * sizeof(double)};

            mio->dispatch(*md, std::move(field_vals), Message::Tag::Field);
        },
        mio);
}


int multio_write_grib_encoded(multio_handle_t* mio, void* gribdata, int gribsize) {
    return wrapApiFunction(
        [mio, gribdata, gribsize]() {
            ASSERT(mio);
            ASSERT(gribdata);

            multio::message::Metadata md;
            md.set("format", "grib");

            mio->dispatch(std::move(md), eckit::Buffer{gribdata, gribsize * sizeof(char)}, Message::Tag::Field);
        },
        mio);
}


int multio_new_metadata(multio_metadata_t** md, multio_handle_t* mio) {
    return wrapApiFunction([md]() { (*md) = new multio_metadata_t{}; }, mio);
}

int multio_copy_metadata(multio_metadata_t** md, multio_metadata_t* mdFrom) {
    return wrapApiFunction([md, mdFrom]() { (*md) = new multio_metadata_t{*mdFrom}; }, mdFrom);
}

int multio_delete_metadata(multio_metadata_t* md) {
    return wrapApiFunction(
        [md]() {
            ASSERT(md);
            delete md;
        },
        md);
}


int multio_metadata_set_int(multio_metadata_t* md, const char* key, int value) {
    return wrapApiFunction(
        [md, key, value]() {
            ASSERT(md);
            ASSERT(key);

            md->set(key, value);
        },
        md);
}

int multio_metadata_set_long(multio_metadata_t* md, const char* key, long value) {
    return wrapApiFunction(
        [md, key, value]() {
            ASSERT(md);
            ASSERT(key);

            md->set(key, value);
        },
        md);
}

int multio_metadata_set_longlong(multio_metadata_t* md, const char* key, long long value) {
    return wrapApiFunction(
        [md, key, value]() {
            ASSERT(md);
            ASSERT(key);

            md->set(key, value);
        },
        md);
}

int multio_metadata_set_string(multio_metadata_t* md, const char* key, const char* value) {
    return wrapApiFunction(
        [md, key, value]() {
            ASSERT(md);
            ASSERT(key);
            ASSERT(value);

            md->set(key, value);
        },
        md);
}

int multio_metadata_set_bool(multio_metadata_t* md, const char* key, bool value) {
    return wrapApiFunction(
        [md, key, value]() {
            ASSERT(md);
            ASSERT(key);

            md->set(key, value);
        },
        md);
}

int multio_metadata_set_float(multio_metadata_t* md, const char* key, float value) {
    return wrapApiFunction(
        [md, key, value]() {
            ASSERT(md);
            ASSERT(key);

            md->set(key, value);
        },
        md);
}

int multio_metadata_set_double(multio_metadata_t* md, const char* key, double value) {
    return wrapApiFunction(
        [md, key, value]() {
            ASSERT(md);
            ASSERT(key);

            // TODO: it is unclear if we ever need to support setting metadata values as float; even if so, we are
            // probably better off casting to double for storing it in multio::Metadata
            md->set(key, static_cast<double>(value));
        },
        md);
}

int multio_field_accepted(multio_handle_t* mio, const multio_metadata_t* md, bool* accepted) {
    return wrapApiFunction(
        [mio, md, accepted]() {
            ASSERT(mio);
            ASSERT(md);
            ASSERT(accepted);

            *accepted = mio->isFieldMatched(*md);
        },
        mio);
}


}  // extern "C"


// Casting between cpp and c type for testing

Metadata* multio_from_c(multio_metadata_t* md) {
    return static_cast<Metadata*>(md);
}

multio_metadata_t* multio_to_c(Metadata* md) {
    return static_cast<multio_metadata_t*>(md);
}
