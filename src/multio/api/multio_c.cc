
#include "multio_c.h"

#include <functional>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/runtime/Main.h"

#include "multio/multio_version.h"
#include "multio/message/Metadata.h"
#include "multio/server/MultioClient.h"
#include "multio/server/MultioServer.h"
#include "multio/util/ConfigurationPath.h"

using eckit::Log;

using multio::message::Peer;
using multio::message::Message;
using multio::message::Metadata;

using multio::util::configuration_file;

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

template <typename FN>
int wrapApiFunction(FN f) {

    try {
        return innerWrapFn(f);
    } catch (eckit::Exception& e) {
        Log::error() << "Caught eckit exception on C-C++ API boundary: " << e.what() << std::endl;
        g_current_error_str = e.what();
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, MULTIO_ERROR_ECKIT_EXCEPTION);
        }
        return MULTIO_ERROR_ECKIT_EXCEPTION;
    } catch (std::exception& e) {
        Log::error() << "Caught exception on C-C++ API boundary: " << e.what() << std::endl;
        g_current_error_str = e.what();
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, MULTIO_ERROR_GENERAL_EXCEPTION);
        }
        return MULTIO_ERROR_GENERAL_EXCEPTION;
    } catch (...) {
        Log::error() << "Caught unknown on C-C++ API boundary" << std::endl;
        g_current_error_str = "Unrecognised and unknown exception";
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, MULTIO_ERROR_UNKNOWN_EXCEPTION);
        }
        return MULTIO_ERROR_UNKNOWN_EXCEPTION;
    }

    ASSERT(false);
}
}  // namespace

extern "C" {

struct multio_handle_t : public multio::server::MultioClient {
    using multio::server::MultioClient::MultioClient;
    multio_handle_t(const eckit::LocalConfiguration& config) : MultioClient{config} {}
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

int multio_new_handle(multio_handle_t** mio) {
    return wrapApiFunction([mio]() {
        const eckit::LocalConfiguration config{eckit::YAMLConfiguration{configuration_file()}};
        (*mio) = new multio_handle_t{config};
    });
}

int multio_delete_handle(multio_handle_t* mio) {
    return wrapApiFunction([mio]() {
        ASSERT(mio);
        delete mio;
    });
}

int multio_start_server(const char* server_name) {
    return wrapApiFunction([server_name]() {
        const eckit::LocalConfiguration config{eckit::YAMLConfiguration{configuration_file()}};
        auto serverConfig = config.getSubConfiguration(server_name);
        multio::server::MultioServer{serverConfig};
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

int multio_write_step_complete(multio_handle_t* mio, multio_metadata_t* md) {
    return wrapApiFunction([mio, md]() {
        ASSERT(mio);
        ASSERT(md);

        mio->dispatch(std::move(*md), eckit::Buffer{0}, Message::Tag::StepComplete);
    });
}

int multio_write_domain(multio_handle_t* mio, multio_metadata_t* md, int* data, int size) {
    return wrapApiFunction([mio, md, data, size]() {
        ASSERT(mio);
        ASSERT(md);

        eckit::Buffer domain_def{reinterpret_cast<const char*>(data), size * sizeof(int)};
        mio->dispatch(std::move(*md), std::move(domain_def), Message::Tag::Domain);
    });
}

int multio_write_mask(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
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

        mio->dispatch(std::move(*md), std::move(mask_vals), Message::Tag::Mask);
    });
}

int multio_write_field(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size) {
    return wrapApiFunction([mio, md, data, size]() {
        ASSERT(mio);
        ASSERT(md);

        eckit::Buffer field_vals{reinterpret_cast<const char*>(data), size * sizeof(double)};

        mio->dispatch(std::move(*md), std::move(field_vals), Message::Tag::Mask);
    });
}

int multio_new_metadata(multio_metadata_t** md) {
    return wrapApiFunction([md]() {
        (*md) = new multio_metadata_t{};
    });
}

int multio_delete_metadata(multio_metadata_t* md) {
    return wrapApiFunction([md]() {
        ASSERT(md);
        delete md;
    });
}

int multio_metadata_set_int_value(multio_metadata_t* md, const char* key, int value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);

        std::string skey{key};
        md->set(key, value);
    });
}

int multio_metadata_set_string_value(multio_metadata_t* md, const char* key, const char* value) {
    return wrapApiFunction([md, key, value]() {
        ASSERT(md);
        ASSERT(key);
        ASSERT(value);

        std::string skey{key}, svalue{value};
        md->set(skey, svalue);
    });
}

}  // extern "C"
