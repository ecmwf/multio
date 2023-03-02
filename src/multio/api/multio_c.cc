
#include "multio_c.h"
#include "multio_c_cpp_utils.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/runtime/Main.h"

#include "multio/message/Metadata.h"
#include "multio/multio_version.h"
#include "multio/server/MultioClient.h"
#include "multio/server/MultioServer.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/ConfigurationPath.h"
#include "multio/util/FailureHandling.h"

#include <functional>

using eckit::Log;

using multio::message::Message;
using multio::message::Metadata;
using multio::message::Peer;

using multio::util::ClientConfigurationContext;
using multio::util::configuration_file;
using multio::util::configuration_file_name;
using multio::util::configuration_path_name;
using multio::util::ConfigurationContext;
using multio::util::FailureAwareException;
using multio::util::MPIInitInfo;
using multio::util::ServerConfigurationContext;

namespace {

extern "C" {

static std::string g_current_error_str;
static multio_failure_handler_t g_failure_handler = nullptr;
static void* g_failure_handler_context = nullptr;

const char* multio_error_string(int err) {
    switch (err) {
        case MULTIO_SUCCESS:
            return "Success";
        case MULTIO_ERROR_ECKIT_EXCEPTION:
        case MULTIO_ERROR_GENERAL_EXCEPTION:
        case MULTIO_ERROR_UNKNOWN_EXCEPTION:
            return g_current_error_str.c_str();
        default:
            return "<unknown>";
    };
}

}  // extern "C"

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

template <typename FN>
int wrapApiFunction(FN f) {
    try {
        return innerWrapFn(f);
    }
    catch (FailureAwareException& e) {
        std::ostringstream oss;
        oss << "Caught a nested exception on C-C++ API boundary: ";
        oss << e;

        g_current_error_str = oss.str();
        MultioErrorValues error = getNestedErrorValue(e);
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, error);
        } else {
            // Print to cerr and cout to make sure the user knows his problem
            std::cerr << oss.str() << std::endl;
            std::cout << oss.str() << std::endl;
        }
        return error;
    }
    catch (eckit::Exception& e) {
        std::ostringstream oss;
        oss << "Caught eckit exception on C-C++ API boundary: " << e.what() << std::endl;
        g_current_error_str = oss.str();
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, MULTIO_ERROR_ECKIT_EXCEPTION);
        } else {
            // Print to cerr and cout to make sure the user knows his problem
            std::cerr << oss.str() << std::endl;
            std::cout << oss.str() << std::endl;
        }
        return MULTIO_ERROR_ECKIT_EXCEPTION;
    }
    catch (std::exception& e) {
        std::ostringstream oss;
        oss << "Caught exception on C-C++ API boundary: " << e.what() << std::endl;
        g_current_error_str = oss.str();
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, MULTIO_ERROR_GENERAL_EXCEPTION);
        } else {
            // Print to cerr and cout to make sure the user knows his problem
            std::cerr << oss.str() << std::endl;
            std::cout << oss.str() << std::endl;
        }
        return MULTIO_ERROR_GENERAL_EXCEPTION;
    }
    catch (...) {
        g_current_error_str = "Cauth unkown exception on C-C++ API boundary";
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, MULTIO_ERROR_UNKNOWN_EXCEPTION);
        } else {
            // Print to cerr and cout to make sure the user knows his problem
            std::cerr << g_current_error_str << std::endl;
            std::cout << g_current_error_str << std::endl;
        }
        return MULTIO_ERROR_UNKNOWN_EXCEPTION;
    }

    ASSERT(false);
}
}  // namespace

extern "C" {

struct multio_configuration_t : public ConfigurationContext {
    multio_configuration_t(const eckit::PathName& fileName = configuration_file_name()) :
        ConfigurationContext{fileName} {}
};

struct multio_handle_t : public multio::server::MultioClient {
    using multio::server::MultioClient::MultioClient;
    multio_handle_t(const ClientConfigurationContext& confCtx) : MultioClient{confCtx} {}
};

struct multio_metadata_t : public multio::message::Metadata {
    using multio::message::Metadata::Metadata;
};

int multio_initialise() {
    return wrapApiFunction([] {
        static bool initialised = false;

        if (initialised) {
            Log::warning() << "Initialising MultIO library twice" << std::endl;
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

int multio_set_failure_handler(multio_failure_handler_t handler, void* context) {
    return wrapApiFunction([handler, context] {
        g_failure_handler = handler;
        g_failure_handler_context = context;
        eckit::Log::info() << "MultIO setting failure handler callable" << std::endl;
    });
}


int multio_new_configuration(multio_configuration_t** cc) {
    return wrapApiFunction([cc]() { (*cc) = new multio_configuration_t{}; });
};

int multio_new_configuration_from_filename(multio_configuration_t** cc, const char* conf_file_name) {
    return wrapApiFunction([cc, conf_file_name]() {
        (*cc) = new multio_configuration_t{conf_file_name != nullptr ? eckit::PathName{conf_file_name}
                                                                     : configuration_file_name()};
    });
};

int multio_delete_configuration(multio_configuration_t* cc) {
    return wrapApiFunction([cc]() {
        ASSERT(cc);
        delete cc;
    });
};

int multio_conf_set_path(multio_configuration_t* cc, const char* configuration_path) {
    return wrapApiFunction([cc, configuration_path]() {
        ASSERT(cc);
        if (configuration_path != nullptr) {
            cc->setPathName(eckit::PathName(configuration_path));
        }
    });
};

int multio_conf_mpi_allow_world_default_comm(multio_configuration_t* cc, bool allow) {
    return wrapApiFunction([cc, allow]() {
        ASSERT(cc);
        if (!cc->getMPIInitInfo()) {
            cc->setMPIInitInfo(eckit::Optional<MPIInitInfo>{MPIInitInfo{}});
        }
        cc->getMPIInitInfo().value().allowWorldAsDefault = allow;
    });
};

int multio_conf_mpi_parent_comm(multio_configuration_t* cc, int parent_comm) {
    return wrapApiFunction([cc, parent_comm]() {
        ASSERT(cc);
        if (!cc->getMPIInitInfo()) {
            cc->setMPIInitInfo(eckit::Optional<MPIInitInfo>{MPIInitInfo{}});
        }
        cc->getMPIInitInfo().value().parentComm = parent_comm;
    });
};

int multio_conf_mpi_return_client_comm(multio_configuration_t* cc, int* return_client_comm) {
    return wrapApiFunction([cc, return_client_comm]() {
        ASSERT(cc);
        if (!cc->getMPIInitInfo()) {
            cc->setMPIInitInfo(eckit::Optional<MPIInitInfo>{MPIInitInfo{}});
        }
        cc->getMPIInitInfo().value().returnClientComm = return_client_comm;
    });
};

int multio_conf_mpi_return_server_comm(multio_configuration_t* cc, int* return_server_comm) {
    return wrapApiFunction([cc, return_server_comm]() {
        ASSERT(cc);
        if (!cc->getMPIInitInfo()) {
            cc->setMPIInitInfo(eckit::Optional<MPIInitInfo>{MPIInitInfo{}});
        }
        cc->getMPIInitInfo().value().returnServerComm = return_server_comm;
    });
};

int multio_conf_mpi_client_id(multio_configuration_t* cc, const char* client_id) {
    return wrapApiFunction([cc, client_id]() {
        ASSERT(cc);
        if (client_id != nullptr) {
            if (!cc->getMPIInitInfo()) {
                cc->setMPIInitInfo(eckit::Optional<MPIInitInfo>{MPIInitInfo{}});
            }
            cc->getMPIInitInfo().value().clientId = eckit::Optional<std::string>{client_id};
        }
    });
}


int multio_new_handle(multio_handle_t** mio, multio_configuration_t* cc) {
    return wrapApiFunction([mio, cc]() {
        ASSERT(cc);
        (*mio) = new multio_handle_t{ClientConfigurationContext{*cc, "client"}};
    });
}

int multio_delete_handle(multio_handle_t* mio) {
    return wrapApiFunction([mio]() {
        ASSERT(mio);
        // std::cout << "multio_delete_handle" << std::endl;
        delete mio;
    });
}

int multio_start_server(multio_configuration_t* cc) {
    return wrapApiFunction([cc]() {
        ASSERT(cc);
        multio::server::MultioServer{ServerConfigurationContext{*cc, "server"}};
    });
}

int multio_open_connections(multio_handle_t* mio) {
    return wrapApiFunction([mio]() {
        ASSERT(mio);

        mio->openConnections();
    });
}

int multio_close_connections(multio_handle_t* mio) {
    return wrapApiFunction([mio]() {
        ASSERT(mio);

        mio->closeConnections();
    });
}

int multio_flush(multio_handle_t* mio, multio_metadata_t* md) {
    return wrapApiFunction([mio, md]() {
        ASSERT(mio);
        ASSERT(md);

        mio->dispatch(*md, eckit::Buffer{0}, Message::Tag::Flush);
    });
}


int multio_notify(multio_handle_t* mio, multio_metadata_t* md) {
    return wrapApiFunction([mio, md]() {
        ASSERT(mio);
        ASSERT(md);

        mio->dispatch(*md, eckit::Buffer{0}, Message::Tag::Notification);
    });
}


int multio_write_domain(multio_handle_t* mio, multio_metadata_t* md, int* data, int size) {
    return wrapApiFunction([mio, md, data, size]() {
        ASSERT(mio);
        ASSERT(md);

        eckit::Buffer domain_def{reinterpret_cast<const char*>(data), size * sizeof(int)};
        mio->dispatch(*md, std::move(domain_def), Message::Tag::Domain);
    });
}

int multio_write_mask_float(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size) {
    return wrapApiFunction([mio, md, data, size]() {
        ASSERT(mio);
        ASSERT(md);

        std::vector<float> mask_data{data, data + size};
        eckit::Buffer mask_vals{size * sizeof(uint8_t)};
        auto bit = static_cast<uint8_t*>(mask_vals.data());
        for (const auto& mval : mask_data) {
            *bit = static_cast<uint8_t>(mval);
            ++bit;
        }

        mio->dispatch(*md, std::move(mask_vals), Message::Tag::Mask);
    });
}

int multio_write_mask_double(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return wrapApiFunction([mio, md, data, size]() {
        ASSERT(mio);
        ASSERT(md);

        std::vector<double> mask_data{data, data + size};
        eckit::Buffer mask_vals{size * sizeof(uint8_t)};
        auto bit = static_cast<uint8_t*>(mask_vals.data());
        for (const auto& mval : mask_data) {
            *bit = static_cast<uint8_t>(mval);
            ++bit;
        }

        mio->dispatch(*md, std::move(mask_vals), Message::Tag::Mask);
    });
}

int multio_write_field_float(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size) {
    return wrapApiFunction([mio, md, data, size]() {
        ASSERT(mio);
        ASSERT(md);

        md->set("precision", "single");

        eckit::Buffer field_vals{reinterpret_cast<const char*>(data), size * sizeof(float)};

        mio->dispatch(*md, std::move(field_vals), Message::Tag::Field);
    });
}

int multio_write_field_double(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return wrapApiFunction([mio, md, data, size]() {
        ASSERT(mio);
        ASSERT(md);

        md->set("precision", "double");

        eckit::Buffer field_vals{reinterpret_cast<const char*>(data), size * sizeof(double)};

        mio->dispatch(*md, std::move(field_vals), Message::Tag::Field);
    });
}

int multio_new_metadata(multio_metadata_t** md) {
    return wrapApiFunction([md]() { (*md) = new multio_metadata_t{}; });
}


int multio_delete_metadata(multio_metadata_t* md) {
    return wrapApiFunction([md]() {
        ASSERT(md);
        delete md;
    });
}


int multio_metadata_set_int(multio_metadata_t* md, const char* key, int value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);

        md->set(key, value);
    });
}

int multio_metadata_set_long(multio_metadata_t* md, const char* key, long value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);

        md->set(key, value);
    });
}

int multio_metadata_set_longlong(multio_metadata_t* md, const char* key, long long value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);

        md->set(key, value);
    });
}

int multio_metadata_set_string(multio_metadata_t* md, const char* key, const char* value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);
        ASSERT(value);

        md->set(key, value);
    });
}

int multio_metadata_set_bool(multio_metadata_t* md, const char* key, bool value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);

        md->set(key, value);
    });
}

int multio_metadata_set_float(multio_metadata_t* md, const char* key, float value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);

        md->set(key, value);
    });
}

int multio_metadata_set_double(multio_metadata_t* md, const char* key, double value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);

        // TODO: it is unclear if we ever need to support setting metadata values as float; even if so, we are probably
        // better off casting to double for storing it in multio::Metadata
        md->set(key, static_cast<double>(value));
    });
}

int multio_field_accepted(multio_handle_t* mio, const multio_metadata_t* md, bool* accepted) {
    return wrapApiFunction([mio, md, accepted]() {
        ASSERT(mio);
        ASSERT(md);
        ASSERT(accepted);

        *accepted = mio->isFieldMatched(*md);
    });
}


}  // extern "C"


// Casting between cpp and c type for testing

Metadata* multio_from_c(multio_metadata_t* md) {
    return static_cast<Metadata*>(md);
}

multio_metadata_t* multio_to_c(Metadata* md) {
    return static_cast<multio_metadata_t*>(md);
}
