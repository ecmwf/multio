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

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/JSON.h"

#include "multio/util/print_buffer.h"
#include "multio/LibMultio.h"
#include "multio/server/Listener.h"
#include "multio/server/ThreadTransport.h"

using multio::server::Listener;
using multio::message::Message;
using multio::message::Metadata;
using multio::print_buffer;
using multio::server::Transport;
using multio::server::TransportFactory;

namespace {
eckit::PathName configuration_path() {
    eckit::PathName base = (::getenv("MULTIO_SERVER_PATH"))
                               ? eckit::PathName{::getenv("MULTIO_SERVER_PATH")}
                               : eckit::PathName{""};

    return base + "/configs/";
}

eckit::LocalConfiguration test_configuration(const std::string& type) {
    eckit::Log::debug<multio::LibMultio>() << "Transport type: " << type << std::endl;

    std::map<std::string, std::string> configs = {{"mpi", "mpi-test-configuration"},
                                                  {"tcp", "tcp-test-configuration"},
                                                  {"thread", "thread-test-configuration"},
                                                  {"none", "no-transport-test-configuration"}};

    eckit::YAMLConfiguration testConfigs{configuration_path() + "test-configurations.yaml"};
    return eckit::LocalConfiguration{testConfigs.getSubConfiguration(configs.at(type))};
}

}  // namespace

class IoTransport {
private:
    eckit::LocalConfiguration config_;
    std::shared_ptr<Transport> transport_;
    Listener listener_;
    std::thread listenerThread_;

    Metadata metadata_;
    bool isOpen_ = false;

    // Default values -- how we set them will depend on the transport layer
    size_t clientCount_ = 1;
    size_t serverCount_ = 1;
    size_t globalSize_ = 2048;

    IoTransport() :
        config_{test_configuration("thread")},
        transport_{TransportFactory::instance().build("thread", config_)},
        listener_{config_, *transport_},
        listenerThread_{&Listener::listen, &listener_} {}

    ~IoTransport() {
        if (listenerThread_.joinable()) {
            listenerThread_.join();
            std::cout << "*** Joined thread" << std::endl;
        }
    }

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
        return listenerThread_;
    }

    Metadata& metadata() {
        return metadata_;
    }

    bool isOpen() const {
        return isOpen_;
    }

    void open() {
        isOpen_ = true;
    }

    void close() {
        isOpen_ = false;
    }

    void setDimensions(size_t nClient, size_t nServer, size_t glFieldSize) {
        clientCount_ = nClient;
        serverCount_ = nServer;
        globalSize_ = glFieldSize;
    }

    size_t clientCount() { return clientCount_; }

    size_t serverCount() { return serverCount_; }

    size_t globalSize() { return globalSize_; }
};

// C/Fortran nterface

using multio::message::Peer;

#ifdef __cplusplus
    extern "C" {
#endif

void set_multio_dimensions_(fortint* clients, fortint* servers, fortint* glfields) {
    IoTransport::instance().setDimensions(static_cast<size_t>(*clients),
                                          static_cast<size_t>(*servers),
                                          static_cast<size_t>(*glfields));
}

void open_multio_connection_() {
    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    Message open{Message::Header{Message::Tag::Open, client, server}, std::string("open")};
    IoTransport::instance().transport().send(open);
}

void close_multio_connection_() {
    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    Message close{Message::Header{Message::Tag::Close, client, server}, std::string("close")};
    IoTransport::instance().transport().send(close);
}

void send_multio_step_complete_() {
    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    Message close{Message::Header{Message::Tag::StepComplete, client, server}, std::string("flush")};
    IoTransport::instance().transport().send(close);
}

void open_multio_message_() {
    ASSERT(!IoTransport::instance().isOpen());
    IoTransport::instance().metadata() = Metadata{};
    IoTransport::instance().open();
}

void close_multio_message_() {
    ASSERT(IoTransport::instance().isOpen());
    IoTransport::instance().close();
}

void set_multio_bool_value_(const char* key, bool* value, int key_len) {
    std::string skey{key, key + key_len};
    IoTransport::instance().metadata().set(skey, *value);
}

void set_multio_int_value_(const char* key, fortint* value, int key_len) {
    std::string skey{key, key + key_len};
    IoTransport::instance().metadata().set(skey, *value);
}

void set_multio_int_array_value_(const char* key, fortint* data, fortint* size, int key_len) {
    std::string skey{key, key + key_len};
    static_assert(sizeof(int) == sizeof(fortint), "Type 'int' is not 32-bit long");
    std::vector<int> vvalue{data, data + *size};
    IoTransport::instance().metadata().set(skey, vvalue);
}

void set_multio_real_value_(const char* key, double* value, int key_len) {
    std::string skey{key, key + key_len};
    IoTransport::instance().metadata().set(skey, *value);
}

void set_multio_real_array_value_(const char* key, double* data, fortint* size, int key_len) {
    std::string skey{key, key + key_len};
    std::vector<double> vvalue{data, data + *size};
    IoTransport::instance().metadata().set(skey, vvalue);
}

void set_multio_string_value_(const char* key, const char* value, int key_len, int val_len) {
    std::string skey{key, key + key_len};
    std::string svalue{value, value + val_len};
    IoTransport::instance().metadata().set(skey, svalue);
}

void send_multio_grib_template_(const void* grib_msg, fortint *words) {

    size_t len = (*words) * sizeof(fortint);
    eckit::Buffer buffer{(const char*)(grib_msg), len};

    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};

    std::stringstream field_id;
    eckit::JSON json(field_id);
    json << IoTransport::instance().metadata();

    Message msg{Message::Header{Message::Tag::Grib, client, server, "", "",
                                IoTransport::instance().clientCount(),
                                IoTransport::instance().globalSize(), "", field_id.str()},
                buffer};

    std::cout << "*** Sending message from " << msg.source() << " to " << msg.destination()
              << std::endl;

    IoTransport::instance().transport().send(msg);
}

void send_multio_mapping_(const void* in_ptr, fortint* words, const char* name, int name_len) {
    std::string mapping_name{name, name + name_len};
    std::cout << " ***** Address: " << in_ptr << ", size = " << *words
              << ", mapping_name = " << mapping_name << ", name_len = " << name_len << std::endl;

    auto nb_clients = IoTransport::instance().clientCount();

    const char* ptr = (const char*)(in_ptr);
    auto len = ((*words) / nb_clients) * sizeof(fortint);
    for (int ii = 0; ii != nb_clients; ++ii) {
        Peer client = IoTransport::instance().transport().localPeer();
        Peer server{"thread", std::hash<std::thread::id>{}(
                                  IoTransport::instance().listenerThread().get_id())};

        eckit::Buffer buffer{ptr, len};

        Message msg{Message::Header{Message::Tag::Domain, client, server, mapping_name,
                                    "unstructured", nb_clients},
                    buffer};

        std::cout << "Rank " << ii + 1 << ": local-to-global mapping = ";
        print_buffer((int*)(ptr), len / sizeof(fortint));
        std::cout << std::endl;

        IoTransport::instance().transport().send(msg);

        ptr += len;
    }
}

void send_multio_field_(const double* data, fortint* size, const char* name, const char* cat,
                        fortint name_len, fortint cat_len) {
    std::cout << " ***** Field metadata = " << IoTransport::instance().metadata() << std::endl;

    std::cout << " ***** Field data = ";
    print_buffer(data, *size);
    std::cout << std::endl;

    Peer client = IoTransport::instance().transport().localPeer();
    Peer server{"thread",
                std::hash<std::thread::id>{}(IoTransport::instance().listenerThread().get_id())};
    std::string domain_name{name, name + name_len};
    std::string category{cat, cat + cat_len};

    eckit::Buffer buffer{(const char*)(data), (*size) * sizeof(double)};

    std::stringstream field_id;
    eckit::JSON json(field_id);
    json << IoTransport::instance().metadata();

    auto gribName = IoTransport::instance().metadata().getString("igrib");
    Message msg{Message::Header{Message::Tag::Field, client, server, gribName, category,
                                IoTransport::instance().clientCount(),
                                IoTransport::instance().globalSize(), domain_name, field_id.str()},
                buffer};

    IoTransport::instance().transport().send(msg);
}

#ifdef __cplusplus
}
#endif
