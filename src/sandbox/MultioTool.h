
#ifndef multio_sandbox_MultioTool_H
#define multio_sandbox_MultioTool_H

#include <vector>
#include <thread>
#include <tuple>

#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/runtime/Tool.h"

#include "sandbox/Peer.h"

namespace eckit {
class Configuration;
}

namespace multio {
namespace sandbox {

class Transport;

class MultioTool : public eckit::Tool {
public:  // methods
    static void usage(const std::string& tool);

protected:
    MultioTool(int argc, char** argv);

    std::vector<eckit::option::Option*> options_;

    virtual void init(const eckit::option::CmdArgs& args);

    virtual void finish(const eckit::option::CmdArgs&) {}

    size_t nbClients_ = 1;
    size_t nbServers_ = 1;

private:
    virtual void execute(const eckit::option::CmdArgs& args) = 0;

    virtual int numberOfPositionalArguments() const { return -1; }
    virtual int minimumPositionalArguments() const { return -1; }

    void run() override final;
};

class MultioThreadTool final : public MultioTool {
public:
    MultioThreadTool(int argc, char** argv);

private:
    void execute(const eckit::option::CmdArgs& args) override;

    std::tuple<std::vector<Peer>, std::vector<std::thread>> spawnServers(
        const eckit::Configuration& config, std::shared_ptr<Transport> transport, size_t nbServers);

    std::vector<std::thread> spawnClients(std::shared_ptr<Transport> transport, size_t nbClients,
                                          const std::vector<Peer>& serverPeers);
};

}  // namespace sandbox
}  // namespace multio

#endif
