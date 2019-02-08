
#ifndef multio_sandbox_MultioThreadTool_H
#define multio_sandbox_MultioThreadTool_H

#include <thread>
#include <tuple>

#include "sandbox/Peer.h"
#include "sandbox/MultioTool.h"

namespace eckit {
class Configuration;
}

namespace multio {
namespace sandbox {

class Transport;

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
