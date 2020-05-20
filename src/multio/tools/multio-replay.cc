
#include <fstream>
#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/log/JSON.h"

#include "multio/LibMultio.h"
#include "multio/server/Listener.h"
#include "multio/server/MultioNemo.h"
#include "multio/server/NemoToGrib.h"
#include "multio/server/Transport.h"
#include "multio/tools/MultioTool.h"

using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------

class MultioReplay final : public multio::MultioTool {
public:

    MultioReplay(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    void runClient();

    void setMetadata();
    void setDomains();
    void writeFields();

    std::vector<int> readGrid(const std::string& grid_type, size_t client_id);
    std::vector<double> readField(const std::string& param, size_t client_id) const;

    size_t commSize() const;
    void initClient();
    void testData();

    const NemoToGrib paramMap_;
    const std::vector<std::string> parameters_{"sst", "ssu", "ssv", "ssw"};

    std::string transportType_ = "mpi";
    std::string pathToNemoData_ = "";

    int globalSize_ = 105704;
    int level_ = 1;
    int step_ = 24;
    size_t rank_ = 0;
};

//----------------------------------------------------------------------------------------------------------------

MultioReplay::MultioReplay(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("path", "Path to NEMO data"));
    options_.push_back(new eckit::option::SimpleOption<long>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<long>("field", "Name of field to replay"));
    options_.push_back(
        new eckit::option::SimpleOption<long>("step", "Time counter for the field to replay"));
}

void MultioReplay::init(const eckit::option::CmdArgs& args) {
    args.get("transport", transportType_);
    args.get("path", pathToNemoData_);

    args.get("step", step_);

    initClient();
}

void MultioReplay::finish(const eckit::option::CmdArgs&) {}

void MultioReplay::execute(const eckit::option::CmdArgs &) {
    runClient();

    testData();
 }

 void MultioReplay::runClient() {
     setMetadata();

     multio_open_connections();

     setDomains();

     writeFields();

     multio_close_connections();
 }

void MultioReplay::setMetadata() {
    multio_metadata_set_int_value("globalSize", globalSize_);
    multio_metadata_set_int_value("level", level_);
    multio_metadata_set_int_value("step", step_);
}

void MultioReplay::setDomains() {
    for (std::string grid_type : {"grid_T", "grid_U", "grid_V", "grid_W"}) {

        auto dname = "orca_" + grid_type;

        auto buffer = readGrid(grid_type, rank_);
        auto sz = static_cast<int>(buffer.size());

        multio_set_domain(dname.c_str(), buffer.data(), sz);
    }
}

void MultioReplay::writeFields() {
    for (const auto& param : parameters_) {
        auto buffer = readField(param, rank_);

        auto sz = static_cast<int>(buffer.size());
        multio_write_field(param.c_str(), buffer.data(), sz);
    }
}

std::vector<int> MultioReplay::readGrid(const std::string& grid_type, size_t client_id) {
    std::ostringstream oss;
    oss << grid_type << "_" << std::setfill('0') << std::setw(2) << client_id;

    auto grid = eckit::PathName{pathToNemoData_ + oss.str()};

    std::ifstream infile(std::string{grid.fullName()}.c_str());

    std::string gtype;
    infile >> gtype;
    if (gtype != grid_type) {
        throw eckit::SeriousBug{"Wrong grid is being read: " + grid.fullName()};
    }

    std::vector<int> domain_dims;
    for (int next; infile >> next;) {
        domain_dims.push_back(next);
    }

    ASSERT(globalSize_ == (domain_dims[0] * domain_dims[1]));

    return domain_dims;
}

std::vector<double> MultioReplay::readField(const std::string& param, size_t client_id) const {
    std::ostringstream oss;
    oss << param << "_" << std::setfill('0') << std::setw(2) << step_ << "_" << std::setfill('0')
        << std::setw(2) << client_id;

    auto field = eckit::PathName{pathToNemoData_ + oss.str()};

    std::ifstream infile(std::string{field.fullName()}.c_str());
    infile.seekg(0, infile.end);
    auto bytes = infile.tellg();
    infile.seekg(0, infile.beg);

    std::vector<double> vals(bytes / sizeof(double));
    infile.read(reinterpret_cast<char*>(vals.data()), bytes);

    return vals;
}

void MultioReplay::initClient() {
    if (transportType_ != "mpi") {
        throw eckit::SeriousBug("Only MPI transport is supported for this tool");
    }

    rank_ = eckit::mpi::comm("world").rank();

    multio_init_client("oce", eckit::mpi::comm().communicator());
}

void MultioReplay::testData() {
    eckit::mpi::comm().barrier();
    if (eckit::mpi::comm().rank() != 0) {
        return;
    }

    for (const auto& param : parameters_) {
        std::ifstream infile{std::to_string(level_) +
                             "::" + std::to_string(paramMap_.get(param).param) +
                             "::" + std::to_string(step_)};
        std::string actual{std::istreambuf_iterator<char>(infile),
                           std::istreambuf_iterator<char>()};
        infile.close();

        auto path = eckit::PathName{std::string{pathToNemoData_ + param + "_" +
                                                std::to_string(step_) + "_reference"}};

        infile.open(std::string{path.fullName()}.c_str());
        std::string expected{std::istreambuf_iterator<char>(infile),
                             std::istreambuf_iterator<char>()};

        ASSERT(actual == expected);
    }
}


//----------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioReplay tool(argc, argv);
    return tool.start();
}
