
extern "C" {
#include <maestro.h>
}

#include "eckit/log/Log.h"
#include "eckit/option/SimpleOption.h"

#include "multio/tools/MultioTool.h"

namespace multio {

class MaestroSyphon final : public multio::MultioTool {
public:  // methods
    MaestroSyphon(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    std::string reqFile_ = "";
};

MaestroSyphon::MaestroSyphon(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("file", "File containing the requirements"));
}

void MaestroSyphon::init(const eckit::option::CmdArgs& args) {
    args.get("file", reqFile_);

    mstro_status s =
        mstro_init(::getenv("MSTRO_WORKFLOW_NAME"), ::getenv("MSTRO_COMPONENT_NAME"), 0);
    ASSERT(s == MSTRO_OK);
}

void MaestroSyphon::finish(const eckit::option::CmdArgs&) {
    ASSERT(mstro_finalize() == MSTRO_OK);
}

void MaestroSyphon::execute(const eckit::option::CmdArgs&) {

}

}  // namespace multio

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multio::MaestroSyphon tool(argc, argv);
    return tool.start();
}
