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
#include "multio/server/print_buffer.h"
#include "multio/server/ThreadTransport.h"

using multio::server::Listener;
using multio::server::Message;
using multio::server::Metadata;
using multio::server::plan_configurations;
using multio::server::print_buffer;
using multio::server::Transport;
using multio::server::TransportFactory;

class IoTransport {
public:

    Metadata metadata_;
    bool is_open_ = false;

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

#ifdef __cplusplus
    extern "C" {
#endif

void print_grib_id_(int* gid) {
    printf("  ==== Hello handle: %d\n", *gid);

    // grib_handle *h = get_handle(*gid);

    size_t len;
    grib_f_get_message_size_(gid, &len);

    printf("  ==== Message size: %zu\n", len);
}

void print_grib_handle_(grib_handle* h) {
    printf("  ==== Hello handle: %p\n", h);
}

void open_io_connection_() {
    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    Message open{Message::Header{Message::Tag::Open, client, server}, std::string("open")};
    IoTransport::instance().transport().send(open);
}

void close_io_connection_() {
    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    Message close{Message::Header{Message::Tag::Close, client, server}, std::string("close")};
    IoTransport::instance().transport().send(close);
}

void send_grib_template_(const void* grib_msg, fortint *words) {

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

void send_mapping_(const void* in_ptr, fortint* words, fortint* nb_clients,
                      const char* name, int name_len) {

    std::string mapping_name{name, name + name_len};
    std::cout << " ***** Address: " << in_ptr << ", size = " << *words
              << ", nb_clients = " << *nb_clients << ", mapping_name = " << mapping_name
              << ", name_len = " << name_len << std::endl;

    const char* ptr = (const char*)(in_ptr);
    auto len = ((*words) / (*nb_clients)) * sizeof(fortint);
    for (int ii = 0; ii != *nb_clients; ++ii) {
        Peer client = IoTransport::instance().transport().localPeer();
        Peer server{"thread", std::hash<std::thread::id>{}(
                                  IoTransport::instance().listenerThread().get_id())};

        eckit::Buffer buffer{ptr, len};

        Message msg{
            Message::Header{Message::Tag::Mapping, client, server, mapping_name, size_t(*nb_clients)},
            buffer};

        std::cout << "Rank " << ii + 1 << ": local-to-global mapping = ";
        print_buffer((int*)(ptr), len / sizeof(fortint));
        std::cout << std::endl;

        IoTransport::instance().transport().send(msg);

        ptr += len;
    }
}

void open_multio_message_() {
    ASSERT(!IoTransport::instance().is_open_);
    IoTransport::instance().metadata_ = Metadata{};
    IoTransport::instance().is_open_ = true;
}

void close_multio_message_() {
    ASSERT(IoTransport::instance().is_open_);
    IoTransport::instance().is_open_ = false;
    std::cout << " ***** Field metadata = " << IoTransport::instance().metadata_ << std::endl;
}

void set_multio_bool_value_(const char* key, bool* value, int key_len) {
    std::string skey{key, key + key_len};
    IoTransport::instance().metadata_.set(skey, *value);
}

void set_multio_int_value_(const char* key, fortint* value, int key_len) {
    std::string skey{key, key + key_len};
    IoTransport::instance().metadata_.set(skey, *value);
}

void set_multio_real_value_(const char* key, double* value, int key_len) {
    std::string skey{key, key + key_len};
    IoTransport::instance().metadata_.set(skey, *value);
}

void set_multio_string_value_(const char* key, const char* value, int key_len, int val_len) {
    std::string skey{key, key + key_len};
    std::string svalue{value, value + val_len};
    IoTransport::instance().metadata_.set(skey, svalue);
}

#ifdef __cplusplus
}
#endif
