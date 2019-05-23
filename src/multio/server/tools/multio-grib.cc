
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "eccodes.h"
#include "grib_api_internal.h"

#include "multio/server/IoTransport.h"
#include "multio/server/MultioServerTool.h"

using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------------

class MultioGrib final : public multio::server::MultioServerTool {
public:  // methods

    MultioGrib(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;
};

//----------------------------------------------------------------------------------------------------------------------

MultioGrib::MultioGrib(int argc, char** argv) : multio::server::MultioServerTool(argc, argv) {}

void MultioGrib::init(const eckit::option::CmdArgs& args) {}

namespace {

void create_grib_handle(const std::string &name) {

    grib_context *c = grib_context_get_default();
    c->grib_samples_path = const_cast<char *>("/tmp/masd/build/ifs-bundle/share/eccodes/ifs_samples/grib1_mlgrib2");

    grib_handle *h = grib_handle_new_from_samples(c, name.c_str());

    print_grib_handle_(h);
}
}

void MultioGrib::execute(const eckit::option::CmdArgs &) {

    create_grib_handle("gg_sfc_grib1");
    create_grib_handle("gg_sfc_grib2");
    create_grib_handle("gg_ml");
    create_grib_handle("sh_sfc");
    create_grib_handle("sh_ml");
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioGrib tool(argc, argv);
    return tool.start();
}
