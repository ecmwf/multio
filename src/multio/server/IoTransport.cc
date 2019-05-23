/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "IoTransport.h"

#include <memory>
#include <typeinfo>

#include "../../../../eccodes/fortran/grib_fortran_prototypes.h"

#include "eckit/config/YAMLConfiguration.h"

#include "multio/server/Listener.h"
#include "multio/server/PlanConfigurations.h"
#include "multio/server/ThreadTransport.h"

using multio::server::Listener;
using multio::server::plan_configurations;
using multio::server::Transport;
using multio::server::TransportFactory;

class IoTransport {
public:

    static IoTransport& instance() {
        static IoTransport transport;
        return  transport;
    }

    Transport& transport() {
        ASSERT(transport_);
        return *transport_;
    }

    const std::thread& listenerThread() {
        return listener_thread_;
    }

private:
    IoTransport() :
        config_{eckit::YAMLConfiguration{plan_configurations("thread")}},
        transport_{TransportFactory::instance().build("thread", config_)},
        listener_{config_, *transport_},
        listener_thread_{&Listener::listen, &listener_} {}

    ~IoTransport() {
        if (listener_thread_.joinable()) {
            listener_thread_.join();
            std::cout << "*** Joined thread" << std::endl;
        }
    }

    eckit::LocalConfiguration config_;
    std::shared_ptr<Transport> transport_;
    Listener listener_;
    std::thread listener_thread_;
};

// C/Fortran nterface

using multio::server::Peer;
using multio::server::Message;

#ifdef __cplusplus
    extern "C" {
#endif

fortint print_grib_id_(int* gid) {
    printf("  ==== Hello handle: %d\n", *gid);

    // grib_handle *h = get_handle(*gid);

    size_t len;
    grib_f_get_message_size_(gid, &len);

    printf("  ==== Message size: %u\n", len);
}

fortint print_grib_handle_(grib_handle* h) {
    printf("  ==== Hello handle: %p\n", h);
}

fortint open_io_connection_() {
    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    Message open{Message::Header{Message::Tag::Open, client, server}, std::string("open")};
    IoTransport::instance().transport().send(open);
}

fortint close_io_connection_() {
    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    Message close{Message::Header{Message::Tag::Close, client, server}, std::string("close")};
    IoTransport::instance().transport().send(close);
}

fortint send_grib_message_(const void* grib_msg, fortint *words) {

    size_t len = (*words) * sizeof(fortint);
    eckit::Buffer buffer{(const char*)(grib_msg), len};

    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    Message msg{Message::Header{Message::Tag::GribTemplate, client, server}, buffer};
    std::cout << "*** Sending message from " << msg.source() << " to " << msg.destination()
              << std::endl;
    IoTransport::instance().transport().send(msg);
}

#ifdef __cplusplus
}
#endif
