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

#include "multio/multio_version.h"
#include "multio/LibMultio.h"
#include "multio/message/Metadata.h"
#include "multio/server/MultioClient.h"
#include "multio/server/MultioErrorHandling.h"
#include "multio/server/MultioServer.h"
#include "multio/server/NemoToGrib.h"
#include "multio/util/ConfigurationPath.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/print_buffer.h"

using multio::message::Peer;
using multio::message::Message;
using multio::message::Metadata;
using multio::util::configuration_file;
using multio::util::configuration_file_name;
using multio::util::configuration_path_name;
using multio::util::MPIInitInfo;
using multio::util::ConfigurationContext;
using multio::util::ClientConfigurationContext;
using multio::util::ServerConfigurationContext;
using multio::util::print_buffer;
using multio::server::MultioClient;
using multio::server::MultioServer;

using NemoKey = std::string;

namespace {
struct GribData {
    long param;
    std::string gridType;
};

}  // namespace

class MultioNemo {
    ConfigurationContext confCtx_;

    // Nemo to grib dictionary
    NemoToGrib paramMap_;

    Metadata metadata_;

    std::unique_ptr<MultioClient> multioClient_ = nullptr;
    std::unique_ptr<MultioServer> multioServer_ = nullptr;

    // Default values -- how we set them will depend on the transport layer

    size_t clientCount_ = 1;
    size_t serverCount_ = 0;

    const double missingValue_ = 0.0;
    const bool bitmapPresent_ = false;
    const long bitsPerValue_ = 16;

    MultioNemo() :
        confCtx_(configuration_file(), configuration_path_name(), configuration_file_name()) {
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
        MPIInitInfo initInfo;
        initInfo.parentComm = eckit::Optional<int>{parent_comm};
        initInfo.clientId = eckit::Optional<std::string>(oce_str);
        confCtx_.setMPIInitInfo(eckit::Optional<MPIInitInfo>(std::move(initInfo)));

        eckit::mpi::addComm("nemo", parent_comm);

        // TODO: find a way to come up with a unique 'colour' -- like getting application number
        const eckit::mpi::Comm& chld = eckit::mpi::comm("nemo").split(777, oce_str);

        auto ret_comm = chld.communicator();

        eckit::Log::info() << "*** Client -- Split nemo communicator " << oce_str
                           << "(parent=" << parent_comm
                           << ",size=" << eckit::mpi::comm("nemo").size() << "; child=" << ret_comm
                           << ",size=" << chld.size() << ")" << std::endl;

        clientCount_ = eckit::mpi::comm(oce_str.c_str()).size();
        serverCount_ = eckit::mpi::comm("nemo").size() - clientCount_;

        multioClient_.reset(new MultioClient{ClientConfigurationContext{confCtx_, "client"}});

        return ret_comm;
    }

    void initServer(int parent_comm, const std::string server_name = "server") {
        MPIInitInfo initInfo;
        initInfo.parentComm = eckit::Optional<int>{parent_comm};
        initInfo.clientId = eckit::Optional<std::string>{};
        confCtx_.setMPIInitInfo(eckit::Optional<MPIInitInfo>(std::move(initInfo)));
        
        eckit::mpi::addComm("nemo", parent_comm);

        // TODO: find a way to come up with a unique 'colour', such as using MPI_APPNUM
        eckit::mpi::comm("nemo").split(888, "server_comm");
        auto server_comm = eckit::mpi::comm("server_comm").communicator();

        eckit::Log::info() << "*** Server -- split nemo communicator server_comm(parent="
                           << parent_comm << ",size=" << eckit::mpi::comm("nemo").size()
                           << "; child=" << server_comm
                           << ",size=" << eckit::mpi::comm("server_comm").size() << ")"
                           << std::endl;

        auto serverConfig = ServerConfigurationContext(confCtx_, server_name);

        multioServer_.reset(new MultioServer{serverConfig});
    }

    void openConnections() {
        MultioNemo::instance().client().openConnections();
    }

    void closeConnections() {
        MultioNemo::instance().client().closeConnections();
    }

    void writeStepComplete() {
        Message msg{Message::Header{Message::Tag::StepComplete, Peer{}, Peer{}}};

        MultioNemo::instance().client().dispatch(msg);
    }

    void setDomain(const std::string& dname, const int* data, size_t bytes) {
        eckit::Buffer domain_def{reinterpret_cast<const char*>(data), bytes};
        Metadata md;
        md.set("name", dname);
        md.set("category", "ocean-domain-map");
        md.set("representation", "structured");
        md.set("toAllServers", true);

        Message msg{Message::Header{Message::Tag::Domain, Peer{}, Peer{}, std::move(md)},
                std::move(domain_def)};

        MultioNemo::instance().client().dispatch(msg);
    }

    void writeMask(const std::string& mname, const uint8_t* data, size_t bytes) {
        metadata_.set("name", mname);
        eckit::Buffer mask_vals{reinterpret_cast<const char*>(data), bytes};
        Metadata md;
        md.set("globalSize", MultioNemo::instance().metadata().getInt("globalSize"));
        md.set("name", mname);
        md.set("category", "ocean-mask");
        md.set("representation", "structured");
        md.set("domain", mname.substr(0, 1) + " grid");
        md.set("levelCount", metadata_.getLong("levelCount"));
        md.set("level", metadata_.getLong("level"));

        md.set("toAllServers", true);
        Message msg{Message::Header{Message::Tag::Mask, Peer{}, Peer{}, std::move(md)},
                std::move(mask_vals)};

        MultioNemo::instance().client().dispatch(msg);
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
        metadata_.set("domain", paramMap_.get(fname).gridType);
        metadata_.set("typeOfLevel", paramMap_.get(fname).levelType);

        // TODO: May not need to be a field's metadata
        metadata_.set("missingValue", missingValue_);
        metadata_.set("bitmapPresent", bitmapPresent_);
        metadata_.set("bitsPerValue", bitsPerValue_);

        eckit::Buffer field_vals{reinterpret_cast<const char*>(data), bytes};

        metadata_.set("toAllServers", to_all_servers);
        Message msg{Message::Header{Message::Tag::Field, Peer{}, Peer{}, std::move(metadata_)},
                    std::move(field_vals)};

        MultioNemo::instance().client().dispatch(msg);
    }

    bool useServer() const {
        return serverCount_ > 0;
    }

    bool isActive(const std::string& name) const {
        return MultioNemo::instance().client().isFieldActive(name);
    }
};

#ifdef __cplusplus
extern "C" {
#endif

void multio_version(const char** version) {
    try {
        *version = multio_version_str();
    }  catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_vcs_version(const char** sha1) {
    try {
        *sha1 = multio_git_sha1();
    }  catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_open_connections() {
    try {
        MultioNemo::instance().openConnections();
    }  catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_close_connections() {
    try {
        MultioNemo::instance().closeConnections();
    }  catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_write_step_complete() {
    try {
        MultioNemo::instance().writeStepComplete();
    }
    catch (std::exception& e) {
        multio_handle_error(e);
    }
}

int multio_init_client(const char* name, int parent_comm) {
    try {
        return MultioNemo::instance().initClient(name, parent_comm);
    }
    catch (std::exception& e) {
        return multio_handle_error(e);
    }
}

void multio_init_server(int parent_comm) {
    try {
        MultioNemo::instance().initServer(parent_comm);
    }
    catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_metadata_set_int_value(const char* key, int value) {
    try {
        std::string skey{key};
        MultioNemo::instance().metadata().set(skey, value);
    }
    catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_metadata_set_string_value(const char* key, const char* value) {
    try {
        std::string skey{key}, svalue{value};
        MultioNemo::instance().metadata().set(skey, svalue);
    }
    catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_set_domain(const char* name, int* data, int size) {
    try {
        if (MultioNemo::instance().useServer()) {
            MultioNemo::instance().setDomain(name, data, size * sizeof(int));
        }
    }
    catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_write_mask(const char* name, const double* data, int size) {
    try {
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
    catch (std::exception& e) {
        multio_handle_error(e);
    }
}

void multio_write_field(const char* name, const double* data, int size, bool to_all_servers) {
    try {
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
    catch (std::exception& e) {
        multio_handle_error(e);
    }
}

bool multio_field_is_active(const char* name) {
    try {
        return MultioNemo::instance().isActive(name);
    }
    catch (std::exception& e) {
        return multio_handle_error(e);
    }
}

void multio_not_implemented(const char* message) {
    try {
        eckit::Log::info() << std::string{message} + " is not currently implemented in MultIO"
                           << std::endl;
    }
    catch (std::exception& e) {
        multio_handle_error(e);
    }
}


#ifdef __cplusplus
}
#endif
