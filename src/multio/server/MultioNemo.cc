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
#include <set>
#include <typeinfo>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/runtime/Main.h"

#include "multio/LibMultio.h"
#include "multio/server/Metadata.h"
#include "multio/server/MultioClient.h"
#include "multio/server/MultioServer.h"
#include "multio/server/print_buffer.h"

using multio::server::Metadata;
using multio::server::print_buffer;
using multio::server::MultioClient;
using multio::server::MultioServer;

namespace {
eckit::PathName configuration_path() {
    eckit::PathName base = (::getenv("MULTIO_SERVER_PATH"))
                               ? eckit::PathName{::getenv("MULTIO_SERVER_PATH")}
                               : eckit::PathName{""};

    return base + "/configs/";
}

// Will come from the configuration
std::set<std::string> active_fields{"sst", "ssu", "ssv", "ssh"};

}  // namespace

class MultioNemo {
    eckit::LocalConfiguration config_;
    Metadata metadata_;

    std::unique_ptr<MultioClient> multioClient_ = nullptr;
    std::unique_ptr<MultioServer> multioServer_ = nullptr;

    // Default values -- how we set them will depend on the transport layer

    size_t clientCount_ = 1;
    size_t serverCount_ = 0;

    size_t writeFrequency_ = 6; // TODO: coming from a configuration

    MultioNemo() : config_{eckit::YAMLConfiguration{configuration_path() + "multio-client.yaml"}} {
        static const char* argv[2] = {"MultioNemo", 0};
        eckit::Main::initialise(1, const_cast<char**>(argv));
    }

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

    int initClient(const std::string& oce_str, int parent_comm) {

        eckit::mpi::addComm("nemo", parent_comm);

        // TODO: find a way to come up with a unique 'colour' -- like getting application number
        const eckit::mpi::Comm& chld = eckit::mpi::comm("nemo").split(777, oce_str);

        auto ret_comm = chld.communicator();

        clientCount_ = eckit::mpi::comm(oce_str.c_str()).size();
        serverCount_ = eckit::mpi::comm("nemo").size() - clientCount_;

        config_.set("group", "nemo");
        config_.set("clientCount", clientCount_);
        config_.set("serverCount", serverCount_);

        multioClient_.reset(new MultioClient{config_});

        return ret_comm;
    }

    void initServer(int nemo_comm) {
        eckit::mpi::addComm("nemo", nemo_comm);

        // TODO: find a way to come up with a unique 'colour'
        eckit::mpi::comm("nemo").split(888, "server_comm");

        multioServer_.reset(new MultioServer{
            eckit::YAMLConfiguration{configuration_path() + "multio-server.yaml"}});
    }

    void setDomain(const std::string& dname, const int* data, size_t bytes) {
        eckit::Buffer domain_def{reinterpret_cast<const char*>(data), bytes};
        client().sendDomain(dname, "structured", domain_def);
    }

    void writeField(const std::string& fname, const double* data, size_t bytes) {
        if (not isActive(fname)) {
            return;
        }

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
        return (active_fields.find(name) != end(active_fields));
    }
};

#ifdef __cplusplus
extern "C" {
#endif

void multio_open_connections() {
    MultioNemo::instance().client().openConnections();
}

void multio_close_connections() {
    MultioNemo::instance().client().closeConnections();
}

void multio_write_step_complete() {
    MultioNemo::instance().client().sendStepComplete();
}

int multio_init_client(const char* name, int parent_comm) {
    return MultioNemo::instance().initClient(name, parent_comm);
}

void multio_init_server(int nemo_comm) {
    MultioNemo::instance().initServer(nemo_comm);
}

void multio_metadata_set_int_value(const char* key, int value) {
    std::string skey{key};
    MultioNemo::instance().metadata().set(skey, value);
}

void multio_set_domain(const char* name, int* data, int size) {
    if (MultioNemo::instance().useServer()) {
        MultioNemo::instance().setDomain(name, data, size * sizeof(int));
    }
    else {
        eckit::Log::debug<multio::LibMultio>() << name << ":  " << std::flush;
        std::vector<int> grid_data{data, data + size};
        print_buffer(grid_data, eckit::Log::debug<multio::LibMultio>());
        eckit::Log::debug<multio::LibMultio>() << std::endl;
    }
}

void multio_write_field(const char* name, const double* data, int size) {
    if (MultioNemo::instance().useServer()) {
        MultioNemo::instance().writeField(name, data, size * sizeof(double));
    }
    else {
        eckit::Log::debug<multio::LibMultio>() << "Writing field " << name << std::endl;
    }
}

bool multio_field_is_active(const char* name) {
    return MultioNemo::instance().isActive(name);
}

#ifdef __cplusplus
}
#endif
