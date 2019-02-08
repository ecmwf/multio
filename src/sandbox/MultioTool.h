
#ifndef multio_sandbox_MultioTool_H
#define multio_sandbox_MultioTool_H

#include <vector>

#include "eckit/runtime/Tool.h"

namespace eckit {
class Configuration;

namespace option {
class CmdArgs;
class Option;
}  // namespace option
}  // namespace eckit

namespace multio {
namespace sandbox {

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

}  // namespace sandbox
}  // namespace multio

#endif
