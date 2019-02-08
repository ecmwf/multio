
#include "MultioTool.h"

#include <algorithm>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "sandbox/Listener.h"
#include "sandbox/ThreadTransport.h"

namespace multio {
namespace sandbox {

MultioTool::MultioTool(int argc, char** argv) : eckit::Tool(argc, argv, "MULTIO_HOME") {
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbservers", "Number of servers"));
}

void MultioTool::usage(const std::string& tool) {
    eckit::Log::info() << std::endl
                       << "Usage: " << tool << " [options]" << std::endl
                       << std::endl
                       << std::endl
                       << "Examples:" << std::endl
                       << "=========" << std::endl
                       << std::endl
                       << tool << " --nbclients=10 --nbservers=4" << std::endl
                       << std::endl;
}

void MultioTool::init(const eckit::option::CmdArgs& args) {
    args.get("nbclients", nbClients_);
    args.get("nbservers", nbServers_);
}

void MultioTool::run() {
    eckit::option::CmdArgs args(&MultioTool::usage, options_, numberOfPositionalArguments(),
                                minimumPositionalArguments());

    init(args);
    execute(args);
    finish(args);
}

}  // namespace sandbox
}  // namespace multio
