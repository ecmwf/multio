
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

    virtual void usage(const std::string &tool) const {
        Log::info() << std::endl
                    << "Usage: " << tool << " [options]" << std::endl
                    << std::endl
                    << std::endl
                    << "Examples:" << std::endl
                    << "=========" << std::endl
                    << std::endl
                    << tool << " --nbclients 10 --nbservers 4" << std::endl
                    << std::endl;
    }

protected:  // methods
    virtual void init(const eckit::option::CmdArgs& args);

private:  // methods

    virtual void execute(const eckit::option::CmdArgs& args);

private:  // members
    size_t nbClients_ = 1;
};

TcpExample::TcpExample(int argc, char** argv) : multio::sandbox::MultioServerTool(argc, argv) {
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
}

void TcpExample::init(const option::CmdArgs& args) {
    args.get("nbclients", nbClients_);
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

void TcpExample::execute(const eckit::option::CmdArgs&) {

    eckit::YAMLConfiguration config{local_plan()};

}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    TcpExample tool(argc, argv);
    return tool.start();
}
