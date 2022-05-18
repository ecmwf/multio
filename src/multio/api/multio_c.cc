
#include "multio_c.h"

#include <functional>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/multio_version.h"
#include "multio/message/Metadata.h"
#include "multio/server/MultioClient.h"
#include "multio/util/ConfigurationPath.h"

using eckit::Log;

using multio::message::Peer;
using multio::message::Message;
using multio::message::Metadata;

using multio::util::configuration_file;

namespace {

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
        return MULTIO_ERROR_ECKIT_EXCEPTION;
    } catch (std::exception& e) {
        Log::error() << "Caught exception on C-C++ API boundary: " << e.what() << std::endl;
        return MULTIO_ERROR_GENERAL_EXCEPTION;
    } catch (...) {
        Log::error() << "Caught unknown on C-C++ API boundary" << std::endl;
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

int multio_version(const char** version) {
    return wrapApiFunction([version]() { (*version) = multio_version_str(); });
}

int multio_vcs_version(const char** sha1) {
    return wrapApiFunction([sha1]() { (*sha1) = multio_git_sha1(); });
}

int multio_new_handle(multio_handle_t** multio) {
    return wrapApiFunction([multio]() {
        const eckit::LocalConfiguration config{eckit::YAMLConfiguration{configuration_file()}};
        (*multio) = new multio_handle_t{config};
    });
}

int multio_delete_handle(multio_handle_t* multio) {
    return wrapApiFunction([multio]() {
        ASSERT(multio);
        delete multio;
    });
}

int multio_open_connections(multio_handle_t* multio) {
    return wrapApiFunction([multio]() { multio->openConnections(); });
}

int multio_close_connections(multio_handle_t* multio) {
    return wrapApiFunction([multio]() { multio->closeConnections(); });
}

int multio_write_step_complete(multio_handle_t* multio) {
    return wrapApiFunction([multio]() {
        Message msg{Message::Header{Message::Tag::StepComplete, Peer{}, Peer{}}};
        multio->dispatch(msg);
    });
}

} // extern "C"
