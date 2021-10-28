
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
#include "multio/server/ConfigurationPath.h"
#include "multio/server/MultioServer.h"
#include "multio/server/Transport.h"
#include "multio/tools/MultioTool.h"

using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------

namespace {
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

//----------------------------------------------------------------------------------------------------------------

class MultioProbe final : public multio::MultioTool {
public:  // methods

    MultioProbe(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    void executeTest();
    void executeLive();

    void testData() const;

    std::string transport_ = "mpi";
    int port_ = 7777;
    bool test_ = false;
    eckit::LocalConfiguration config_;
};

MultioProbe::MultioProbe(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("port", "TCP port"));
    options_.push_back(new eckit::option::SimpleOption<bool>("test", "Whether it runs as part of test"));
}

void MultioProbe::init(const eckit::option::CmdArgs& args) {
    args.get("transport", transport_);
    args.get("port", port_);
    args.get("test", test_);

    config_ = test_configuration(transport_);
    config_.set("local_port", port_);

    if(transport_ == "mpi") {
        if (!eckit::mpi::hasComm("nemo")) {
            int32_t gl_comm = eckit::mpi::comm().communicator();
            eckit::mpi::addComm("nemo", gl_comm);
        }
        // TODO: find a way to come up with a unique 'colour'
        eckit::mpi::comm("nemo").split(888, "server_comm");
    }
}

void MultioProbe::finish(const eckit::option::CmdArgs&) {}

void MultioProbe::execute(const eckit::option::CmdArgs&) {
    if (test_) {
        executeTest();
    }
    else {
        executeLive();
    }
}

//---------------------------------------------------------------------------------------------------------------

void MultioProbe::executeLive() {
    MultioServer server{eckit::YAMLConfiguration{configuration_file()}};
}

void MultioProbe::executeTest() {
    MultioServer server{config_};

    testData();
}

void MultioProbe::testData() const {
    if(transport_ == "mpi") {
        eckit::mpi::comm().barrier();
        return;
    }

    ::sleep(1);
}

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioProbe tool(argc, argv);
    return tool.start();
}
