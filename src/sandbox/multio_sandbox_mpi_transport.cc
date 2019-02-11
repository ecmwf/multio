
#include "eckit/mpi/Comm.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "sandbox/MultioServerTool.h"
#include "sandbox/Listener.h"
#include "sandbox/Message.h"
#include "sandbox/Peer.h"
#include "sandbox/Transport.h"


using eckit::Log;
using namespace multio::sandbox;

//----------------------------------------------------------------------------------------------------------------------

class MpiExample final : public multio::sandbox::MultioServerTool {
public:  // methods

    MpiExample(int argc, char** argv);

    virtual void usage(const std::string &tool) const {
        Log::info() << std::endl
                    << "Usage: " << tool << " [options]" << std::endl
                    << std::endl
                    << std::endl
                    << "Examples:" << std::endl
                    << "=========" << std::endl
                    << std::endl
                    << tool << " --nbservers 4" << std::endl
                    << std::endl;
    }

protected:  // methods
    virtual void init(const eckit::option::CmdArgs& args);

private:  // methods

    virtual void execute(const eckit::option::CmdArgs& args);

private:  // members
    size_t nbClients_ = 1;
};

MpiExample::MpiExample(int argc, char** argv) : multio::sandbox::MultioServerTool(argc, argv) {}

void MpiExample::init(const eckit::option::CmdArgs& args) {
    MultioServerTool::init(args);
    nbClients_ = eckit::mpi::comm("world").size() - nbServers_;
}

//----------------------------------------------------------------------------------------------------------------------

std::string local_plan() {
    return R"json(
    {
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

void MpiExample::execute(const eckit::option::CmdArgs&) {
    eckit::YAMLConfiguration config{local_plan()};

    std::cout << "Clients: " << nbClients_ << std::endl << "Servers: " << nbServers_ << std::endl;

    // std::shared_ptr<Transport> transport{TransportFactory::instance().build("Mpi", config)};
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MpiExample tool(argc, argv);
    return tool.start();
}
