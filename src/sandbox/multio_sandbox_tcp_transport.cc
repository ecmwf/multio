
#include <algorithm>
#include <chrono>
#include <functional>
#include <iostream>
#include <mutex>
#include <string>
#include <thread>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/net/TCPClient.h"
#include "eckit/net/TCPServer.h"
#include "eckit/runtime/Tool.h"

#include "sandbox/MultioServerTool.h"
#include "sandbox/Listener.h"
#include "sandbox/Message.h"
#include "sandbox/Peer.h"
#include "sandbox/Transport.h"

using namespace eckit;
using namespace multio;
using namespace multio::sandbox;

//----------------------------------------------------------------------------------------------------------------------

class TcpExample final : public multio::sandbox::MultioServerTool {
public:  // methods

    TcpExample(int argc, char** argv);

private:
    void usage(const std::string &tool) const override {
        Log::info() << std::endl
                    << "Usage: " << tool << " [options]" << std::endl
                    << std::endl
                    << std::endl
                    << "Examples:" << std::endl
                    << "=========" << std::endl
                    << std::endl
                    << tool << " --port=9771" << std::endl
                    << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    size_t nbClients_ = 1;
    int port_ = 7777;

    eckit::YAMLConfiguration config_;
};

//----------------------------------------------------------------------------------------------------------------------

std::string plan_configurations() {
    return R"json(
        {
           "transport" : "tcp",
           "host" : "skadi",
           "ports" : [9773],
           "plans" : [
              {
                 "name" : "ocean",
                 "actions" : {
                    "root" : {
                       "type" : "Print",
                       "stream" : "error",
                       "next" : {
                          "type" : "AppendToFile",
                          "path" : "messages.txt",
                          "next" : {
                             "type" : "Null"
                          }
                       }
                    }
                 }
              }
           ]
        }
    )json";
}

//----------------------------------------------------------------------------------------------------------------------

TcpExample::TcpExample(int argc, char** argv) :
    multio::sandbox::MultioServerTool(argc, argv),
    config_(plan_configurations()) {
    options_.push_back(new eckit::option::SimpleOption<size_t>("port", "TCP port"));
}

void TcpExample::init(const option::CmdArgs& args) {
    args.get("port", port_);
}

void TcpExample::execute(const eckit::option::CmdArgs&) {
    eckit::LocalConfiguration config{config_};
    config.set("local_port", port_);

    std::shared_ptr<Transport> transport{TransportFactory::instance().build("Tcp", config)};

    auto host = config_.getString("host");
    auto ports = config_.getUnsignedVector("ports");

    if (find(begin(ports), end(ports), port_) != end(ports)) {
        Listener listener{config_, *transport};
        listener.listen();
    }
    else {
        for (auto port : ports) {
            Message msg{{Message::Tag::Open, transport->localPeer(), Peer{host, port}},
                         std::string("open")};
            transport->send(msg);

            std::string str = "Once upon a midnight dreary + " + std::to_string(port);
            msg = Message{{Message::Tag::Field, transport->localPeer(), Peer{host, port}}, str};
            transport->send(msg);

            msg = Message{{Message::Tag::Close, transport->localPeer(), Peer{host, port}},
                          std::string("close")};
            transport->send(msg);
        }
    }
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    TcpExample tool(argc, argv);
    return tool.start();
}
