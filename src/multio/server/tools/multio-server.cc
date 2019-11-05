
#include <unistd.h>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/log/JSON.h"

#include "multio/LibMultio.h"
#include "multio/server/Listener.h"
#include "multio/server/MultioClient.h"
#include "multio/server/MultioServerTool.h"
#include "multio/server/Transport.h"

using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------

namespace {
eckit::PathName base() {
    if (::getenv("MULTIO_SERVER_PATH")) {
        return eckit::PathName{::getenv("MULTIO_SERVER_PATH")};
    }
    return eckit::PathName{""};
}

eckit::PathName test_configuration(const std::string& type) {
    eckit::Log::debug<multio::LibMultio>() << "Transport type: " << type << std::endl;
    std::map<std::string, std::string> configs = {{"mpi", "mpi-test-config.json"},
                                                  {"tcp", "tcp-test-config.json"},
                                                  {"thread", "thread-test-config.json"},
                                                  {"none", "no-transport-test-config.json"}};

    return base() + "/configs/" + eckit::PathName{configs.at(type)};
}
}  // namespace

//----------------------------------------------------------------------------------------------------------------

class MultioServer final : public multio::server::MultioServerTool {
public:  // methods

    MultioServer(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    void testData() const;

    std::string transport_ = "mpi";
    int port_ = 7777;
    bool test_ = false;
    eckit::LocalConfiguration config_;
};

MultioServer::MultioServer(int argc, char** argv) : multio::server::MultioServerTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("port", "TCP port"));
    options_.push_back(new eckit::option::SimpleOption<bool>("test", "Whether it runs as part of test"));
}


void MultioServer::init(const eckit::option::CmdArgs& args) {
    args.get("transport", transport_);
    args.get("port", port_);
    args.get("test", test_);

    config_ = eckit::LocalConfiguration{eckit::YAMLConfiguration{test_configuration(transport_)}};
    config_.set("local_port", port_);

    if(transport_ == "mpi") {
        int32_t gl_comm = eckit::mpi::comm().communicator();
        if (!eckit::mpi::hasComm("nemo")) {
            eckit::mpi::addComm("nemo", gl_comm);
        }
        eckit::mpi::comm("nemo").split(888, "server_comm");
    }
}

void MultioServer::testData() const {
    if(not test_) {
        return;
    }

    if(transport_ == "mpi") {
        eckit::mpi::comm().barrier();
        return;
    }

    ::sleep(1);
}

void MultioServer::execute(const eckit::option::CmdArgs &) {
    std::unique_ptr<Transport> transport{TransportFactory::instance().build(transport_, config_)};

    Listener listener{config_, *transport};
    listener.listen();

    testData();
}

int main(int argc, char** argv) {
    MultioServer tool(argc, argv);
    return tool.start();
}
