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
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/runtime/Main.h"

#include "multio/LibMultio.h"
#include "multio/message/Metadata.h"
#include "multio/server/ConfigurationPath.h"
#include "multio/server/MultioClient.h"
#include "multio/server/MultioServer.h"
#include "multio/server/NemoToGrib.h"
#include "multio/util/print_buffer.h"

using multio::message::Metadata;
using multio::util::print_buffer;
using multio::server::MultioClient;
using multio::server::MultioServer;

using NemoKey = std::string;

namespace {
std::set<std::string> fetch_active_fields(const eckit::Configuration& cfg) {
    const auto& vec = cfg.getStringVector("activeFields");
    return std::set<std::string>{begin(vec), end(vec)};
}

struct GribData {
    long param;
    std::string gridType;
};

}  // namespace

class MultioNemo {
    eckit::LocalConfiguration config_;
    const std::set<std::string> activeFields_;

    // Nemo to grib dictionary
    NemoToGrib paramMap_;

    Metadata metadata_;

    std::unique_ptr<MultioClient> multioClient_ = nullptr;
    std::unique_ptr<MultioServer> multioServer_ = nullptr;

    // Default values -- how we set them will depend on the transport layer

    size_t clientCount_ = 1;
    size_t serverCount_ = 0;

    const double missingValue_ = 0.0;
    const bool bitmapPresent_ = true;
    const long bitsPerValue_ = 16;

    MultioNemo() :
        config_{eckit::YAMLConfiguration{configuration_file()}},
        activeFields_{fetch_active_fields(config_)} {
        static const char* argv[2] = {"MultioNemo", 0};
        eckit::Main::initialise(1, const_cast<char**>(argv));

        metadata_.set("run", config_.getSubConfiguration("run"));
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

        eckit::Log::info() << "*** Split nemo communicator " << oce_str << "(parent=" << parent_comm
                           << ",size=" << eckit::mpi::comm("nemo").size() << "; child=" << ret_comm
                           << ",size=" << chld.size() << ")" << std::endl;

        clientCount_ = eckit::mpi::comm(oce_str.c_str()).size();
        serverCount_ = eckit::mpi::comm("nemo").size() - clientCount_;

        config_.set("group", "nemo");
        config_.set("clientCount", clientCount_);
        config_.set("serverCount", serverCount_);

        multioClient_.reset(new MultioClient{config_});

        return ret_comm;
    }

    void initServer(int parent_comm) {
        eckit::mpi::addComm("nemo", parent_comm);

        // TODO: find a way to come up with a unique 'colour', such as using MPI_APPNUM
        eckit::mpi::comm("nemo").split(888, "server_comm");
        auto server_comm = eckit::mpi::comm("server_comm").communicator();

        eckit::Log::info() << "*** Split nemo communicator server_comm(parent=" << parent_comm
                           << ",size=" << eckit::mpi::comm("nemo").size()
                           << "; child=" << server_comm << ",size="
                           << eckit::mpi::comm("server_comm").size() << ")" << std::endl;

        multioServer_.reset(new MultioServer{eckit::YAMLConfiguration{configuration_file()}});
    }

    void setDomain(const std::string& dname, const int* data, size_t bytes) {
        eckit::Buffer domain_def{reinterpret_cast<const char*>(data), bytes};
        Metadata md;
        md.set("name", dname);
        md.set("category", "structured");
        md.set("domainCount", clientCount_);
        client().sendDomain(std::move(md), std::move(domain_def));
    }

    void writeMask(const std::string& mname, const uint8_t* data, size_t bytes) {
        metadata_.set("name", mname);
        eckit::Buffer mask_vals{reinterpret_cast<const char*>(data), bytes};
        Metadata md;
        md.set("name", mname);
        md.set("category", "structured");
        md.set("domainCount", clientCount_);
        md.set("levelCount", metadata_.getLong("levelCount"));
        md.set("level", metadata_.getLong("level"));

        MultioNemo::instance().client().sendMask(md, std::move(mask_vals));
    }

    void writeField(const std::string& fname, const double* data, size_t bytes,
                    bool to_all_servers = false) {
        if(metadata_.getString("category") != "ocean-grid-coordinate") {
            ASSERT(not to_all_servers); // Sanity check for now -- to remove later
        }

        ASSERT(isActive(fname));

        metadata_.set("name", fname);
        metadata_.set("nemoParam", fname);
        metadata_.set("param", paramMap_.get(fname).param);
        metadata_.set("gridSubtype", paramMap_.get(fname).gridType);
        metadata_.set("domainCount", clientCount_);
        metadata_.set("domain", paramMap_.get(fname).gridType);
        metadata_.set("typeOfLevel", paramMap_.get(fname).levelType);

        // TODO: May not need to be a field's metadata
        metadata_.set("missingValue", missingValue_);
        metadata_.set("bitmapPresent", bitmapPresent_);
        metadata_.set("bitsPerValue", bitsPerValue_);

        eckit::Buffer field_vals{reinterpret_cast<const char*>(data), bytes};

        MultioNemo::instance().client().sendField(metadata_, std::move(field_vals), to_all_servers);
    }

    bool useServer() const {
        return serverCount_ > 0;
    }

    bool isActive(const std::string& name) const {
        return activeFields_.find(name) != end(activeFields_);
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

void multio_init_server(int parent_comm) {
    MultioNemo::instance().initServer(parent_comm);
}

void multio_metadata_set_int_value(const char* key, int value) {
    std::string skey{key};
    MultioNemo::instance().metadata().set(skey, value);
}

void multio_metadata_set_string_value(const char* key, const char* value) {
    std::string skey{key}, svalue{value};
    MultioNemo::instance().metadata().set(skey, svalue);
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

void multio_write_mask(const char* name, const double* data, int size) {
    std::vector<double> mask_data{data, data + size};
    std::vector<uint8_t> bitMask;
    for (const auto& mval : mask_data) {
        if (not (mval == 0.0 || mval == 1.0 || mval == 2.0)) {
            throw eckit::SeriousBug("Unrecognised mask value: " + std::to_string(mval));
        }
        bitMask.push_back(static_cast<uint8_t>(mval));
    }
    ASSERT(bitMask.size() == static_cast<size_t>(size));
    MultioNemo::instance().writeMask(name, bitMask.data(), size * sizeof(uint8_t));
}

void multio_write_field(const char* name, const double* data, int size, bool to_all_servers) {
    if (MultioNemo::instance().useServer()) {
        LOG_DEBUG_LIB(multio::LibMultio)
            << "*** Write field " << name << ", local size = " << size << std::endl;
        MultioNemo::instance().writeField(name, data, size * sizeof(double), to_all_servers);
    }
    else {
        LOG_DEBUG_LIB(multio::LibMultio)
            << "*** Writing field " << name << ", local size = " << size
            << ", global size = " << MultioNemo::instance().metadata().getInt("globalSize")
            << std::endl;
    }
}

bool multio_field_is_active(const char* name) {
    return MultioNemo::instance().isActive(name);
}

void multio_not_implemented(const char* message) {
    eckit::Log::info() << std::string{message} + " is not currently implemented in MultIO"
                       << std::endl;
}


#ifdef __cplusplus
}
#endif
