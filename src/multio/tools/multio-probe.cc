
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
#include "multio/server/MultioServer.h"
#include "multio/util/ConfigurationPath.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/tools/MultioTool.h"

using multio::util::configuration_file;
using multio::util::configuration_file_name;
using multio::util::configuration_path_name;
using multio::util::ConfigurationContext;
using multio::util::ServerConfigurationContext;

using namespace multio::server;

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

    std::string serverName_ = "nemo-ioserver";
    std::string transport_ = "mpi";
    int port_ = 7777;
    bool test_ = false;
    eckit::Optional<ServerConfigurationContext> confCtx_{};
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
    args.get("server", serverName_);
    
    confCtx_ = ServerConfigurationContext(ConfigurationContext(configuration_file(), configuration_path_name(), configuration_file_name()), serverName_);
    confCtx_->config().set("local_port", port_);
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
    eckit::Log::info() << "*** Server -- executeLive "<< std::endl;
    MultioServer server{*confCtx_};
}

void MultioProbe::executeTest() {
    eckit::Log::info() << "*** Server -- executeTest "<< std::endl;
    MultioServer server{*confCtx_};

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
