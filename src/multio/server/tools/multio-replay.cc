
#include <fstream>
#include <iomanip>

#include "eckit/config/YAMLConfiguration.h"
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
#include "multio/server/MultioServerTool.h"
#include "multio/server/Transport.h"

using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------

class MultioReplay final : public multio::server::MultioServerTool {
public:

    MultioReplay(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

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

    std::string transportType_ = "mpi";
    std::string pathToNemoData_ = "";

    int globalSize_ = 105704;
    int level_ = 1;
    int step_ = 24;
    std::map<std::string, std::string> parameters_ = {{"sst", "orca_grid_T"},
                                                      {"ssu", "orca_grid_U"},
                                                      {"ssv", "orca_grid_V"},
                                                      {"ssw", "orca_grid_W"}};

    size_t rank_ = 0;
};

//----------------------------------------------------------------------------------------------------------------

MultioReplay::MultioReplay(int argc, char** argv) : multio::server::MultioServerTool(argc, argv) {
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

void MultioReplay::execute(const eckit::option::CmdArgs &) {
    runClient();

    testData();
 }

 void MultioReplay::runClient() {
     setMetadata();

     multio_open_connection_();

     setDomains();

     writeFields();

     multio_close_connection_();
 }

void MultioReplay::setMetadata() {
    auto key = std::string{"isizeg"};
    multio_metadata_set_int_value_(key.c_str(), &globalSize_, static_cast<int>(key.size()));

    key = std::string{"ilevg"};
    multio_metadata_set_int_value_(key.c_str(), &level_, static_cast<int>(key.size()));

    key = std::string{"istep"};
    multio_metadata_set_int_value_(key.c_str(), &step_, static_cast<int>(key.size()));
}

void MultioReplay::setDomains() {
    for (std::string grid_type : {"grid_T", "grid_U", "grid_V", "grid_W"}) {

        auto dname = "orca_" + grid_type;

        auto buffer = readGrid(grid_type, rank_);
        auto sz = static_cast<int>(buffer.size());

        multio_set_domain_(dname.c_str(), buffer.data(), &sz, static_cast<int>(dname.size()));
    }
}

void MultioReplay::writeFields() {
    for (const auto& param : parameters_) {
        auto buffer = readField(param.first, rank_);

        auto sz = static_cast<int>(buffer.size());
        multio_write_field_(param.first.c_str(), buffer.data(), &sz,
                            static_cast<int>(param.first.size()));
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
        throw eckit::SeriousBug("Wrong grid is being read");
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

    std::string comm_name = "oce";
    int32_t ret_comm;
    int32_t gl_comm = eckit::mpi::comm().communicator();
    multio_init_client_(comm_name.c_str(), &ret_comm, &gl_comm, comm_name.size());
}

void MultioReplay::testData() {
    eckit::mpi::comm().barrier();
    if (eckit::mpi::comm().rank() != 0) {
        return;
    }

    for (const auto& param : parameters_) {
        std::ifstream infile{param.first + "::" + std::to_string(level_) +
                             "::" + std::to_string(step_)};
        std::string actual{std::istreambuf_iterator<char>(infile),
                           std::istreambuf_iterator<char>()};
        infile.close();

        auto path = eckit::PathName{std::string{pathToNemoData_ + param.first + "_" +
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
