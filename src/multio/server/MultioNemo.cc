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
    size_t globalSize_ = 2048;
    size_t depthLevel_ = 1; // normally infer it from field category and levelCount_;
    size_t writeFrequency_ = 6;

    MultioNemo() :
        config_{eckit::YAMLConfiguration{configuration_path()}},
        multioClient_{new MultioClient{config_}} {}

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

    void setDimensions(size_t nClient, size_t nServer, size_t glFieldSize, size_t level) {
        clientCount_ = nClient;
        serverCount_ = nServer;
        globalSize_ = glFieldSize;
        depthLevel_ = level;

        config_.set("clientCount", clientCount_);
        config_.set("serverCount", serverCount_);

        multioClient_.reset(new MultioClient{config_});
    }

    void setDomain(const std::string& dname, const fortint* data, size_t bytes) {
        eckit::Buffer domain_def{reinterpret_cast<const char*>(data), bytes};
        client().sendDomain(dname, "structured", std::move(domain_def));
    }

    void writeField(const std::string& fname, fortint tstep, const double* data, size_t bytes) {
        eckit::Log::debug<multio::LibMultio>()
            << "*** Writing field " << fname << ", step = " << tstep << std::endl;

        if (tstep % writeFrequency_ != 0) {
            return;
        }

        Metadata metadata;
        metadata.set("igrib", fname);
        metadata.set("istep", tstep);
        metadata.set("ilevg", depthLevel_);

        eckit::Buffer field_vals{reinterpret_cast<const char*>(data), bytes};

        MultioNemo::instance().client().sendField(fname, "ocean-surface", globalSize_,
                                                  "orca_grid_T", metadata, std::move(field_vals));
    }

    bool useServer() const {
        return serverCount_ > 0;
    }
};

#ifdef __cplusplus
extern "C" {
#endif

void multio_set_dimensions_(fortint* clients, fortint* servers, fortint* glfields, fortint* level) {
    MultioNemo::instance().setDimensions(
        static_cast<size_t>(*clients), static_cast<size_t>(*servers),
        static_cast<size_t>(*glfields), static_cast<size_t>(*level));
}

void multio_open_connection_() {
    MultioNemo::instance().client().openConnections();
}

void multio_close_connection_() {
    MultioNemo::instance().client().closeConnections();
}

void multio_send_step_complete_() {
    MultioNemo::instance().client().sendStepComplete();
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

void multio_write_field_(const char* fname, const double* data, fortint* size, fortint* timeStep,
                         fortint fn_len) {
    std::string name{fname, fname + fn_len};
    MultioNemo::instance().writeField(name, *timeStep, data, (*size) * sizeof(double));
}

#ifdef __cplusplus
}
#endif
