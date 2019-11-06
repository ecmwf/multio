/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#include "MultioNemo.h"

#include <memory>
#include <typeinfo>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/mpi/Comm.h"

#include "multio/LibMultio.h"
#include "multio/server/Metadata.h"
#include "multio/server/MultioClient.h"
#include "multio/server/print_buffer.h"

using multio::server::Metadata;
using multio::server::print_buffer;
using multio::server::MultioClient;

namespace {
eckit::PathName configuration_path() {
    eckit::PathName base = (::getenv("MULTIO_SERVER_PATH"))
                               ? eckit::PathName{::getenv("MULTIO_SERVER_PATH")}
                               : eckit::PathName{""};

    return base + "/configs/multio-client.json";
}

}  // namespace

class MultioNemo {
    eckit::LocalConfiguration config_;
    Metadata metadata_;

    std::unique_ptr<MultioClient> multioClient_ = nullptr;

    // Default values -- how we set them will depend on the transport layer

    size_t clientCount_ = 1;
    size_t serverCount_ = 0;

    size_t writeFrequency_ = 6; // TODO: coming from a configuration

    MultioNemo() :
        config_{eckit::YAMLConfiguration{configuration_path()}},
        multioClient_{nullptr} {}

public:

    static MultioNemo& instance() {
        static MultioNemo mio;
        return  mio;
    }

    MultioClient& client() {
        ASSERT(multioClient_);
        return *multioClient_;
    }

    Metadata& metadata() {
        return metadata_;
    }

    void initClient(const std::string& group, size_t nClient, size_t nServer) {
        clientCount_ = nClient;
        serverCount_ = nServer;

        config_.set("group", group);
        config_.set("clientCount", clientCount_);
        config_.set("serverCount", serverCount_);

        multioClient_.reset(new MultioClient{config_});
    }

    void setDomain(const std::string& dname, const fortint* data, size_t bytes) {
        eckit::Buffer domain_def{reinterpret_cast<const char*>(data), bytes};
        client().sendDomain(dname, "structured", std::move(domain_def));
    }

    void writeField(const std::string& fname, const double* data, size_t bytes) {
        if (metadata_.getUnsigned("istep") % writeFrequency_ != 0) {
            return;
        }

        metadata_.set("igrib", fname);
        auto gl_size = static_cast<size_t>(metadata_.getInt("isizeg"));

        eckit::Log::debug<multio::LibMultio>()
            << "*** Writing field " << fname << ", step = " << metadata_.getInt("istep")
            << ", level = " << metadata_.getInt("ilevg") << std::endl;

        eckit::Buffer field_vals{reinterpret_cast<const char*>(data), bytes};

        MultioNemo::instance().client().sendField(fname, "ocean-surface", gl_size, "orca_grid_T",
                                                  metadata_, std::move(field_vals));
    }

    bool useServer() const {
        return serverCount_ > 0;
    }

    bool isActive(const std::string& name) const {
        return false; // Not yet implemented
    }
};

#ifdef __cplusplus
extern "C" {
#endif

void multio_open_connection_() {
    MultioNemo::instance().client().openConnections();
}

void multio_close_connection_() {
    MultioNemo::instance().client().closeConnections();
}

void multio_send_step_complete_() {
    MultioNemo::instance().client().sendStepComplete();
}

void multio_metadata_set_int_value_(const char* key, fortint* value, int key_len) {
    std::string skey{key, key + key_len};
    MultioNemo::instance().metadata().set(skey, *value);
}

void multio_init_client_(const char* name, fortint* ret_comm, fortint* gl_comm, int len) {
    // if (config_.getString("transport") = "mpi") {}
    if (!eckit::mpi::hasComm("nemo")) {
        eckit::mpi::addComm("nemo", *gl_comm);
    }
    auto oce_str = std::string{name, name + len};
    eckit::mpi::listComms();
    std::cout << "Calling mpi split " << std::endl;
    // TODO: find a way to come up with a unique 'colour'
    const eckit::mpi::Comm& chld = eckit::mpi::comm("nemo").split(777, oce_str);
    std::cout << "Completed mpi split " << std::endl;
    eckit::mpi::listComms();

    *ret_comm = chld.communicator();

    auto clientCount = eckit::mpi::comm(oce_str.c_str()).size();
    auto serverCount = eckit::mpi::comm("nemo").size() - clientCount;

    MultioNemo::instance().initClient("nemo", clientCount, serverCount);
}

void multio_init_server_(fortint* gl_comm) {
    if (!eckit::mpi::hasComm("nemo")) {
        eckit::mpi::addComm("nemo", *gl_comm);
    }
    std::cout << "Calling mpi split " << std::endl;
    // TODO: find a way to come up with a unique 'colour'
    eckit::mpi::comm("nemo").split(888, "server_comm");
    std::cout << "Completed mpi split " << std::endl;

    eckit::mpi::listComms();
}

void multio_set_domain_(const char* key, fortint* data, fortint* size, fortint key_len) {
    std::string name{key, key + key_len};
    static_assert(sizeof(int) == sizeof(fortint), "Type 'int' is not 32-bit long");
    if (MultioNemo::instance().useServer()) {
        MultioNemo::instance().setDomain(name, data, (*size) * sizeof(fortint));
    }
    else {
        std::cout << name << ":  " << std::flush;
        std::vector<fortint> grid_data{data, data + *size};
        print_buffer(grid_data);
        std::cout << std::endl;
    }
}

void multio_write_field_(const char* fname, const double* data, fortint* size, fortint fn_len) {
    std::string name{fname, fname + fn_len};
    if (MultioNemo::instance().useServer()) {
        MultioNemo::instance().writeField(name, data, (*size) * sizeof(double));
    }
    else {
        std::cout << "Writing field " << name << std::endl;
    }
}

void multio_field_is_active_(const char* fname, bool* is_active, fortint fn_len) {
    std::string name{fname, fname + fn_len};
    *is_active = MultioNemo::instance().isActive(name);
}

#ifdef __cplusplus
}
#endif
