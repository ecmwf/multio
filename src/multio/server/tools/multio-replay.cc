
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
#include "multio/server/MultioClient.h"
#include "multio/server/MultioServerTool.h"
#include "multio/server/Transport.h"

using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------

namespace {
eckit::PathName base() {
    if (::getenv("MULTIO_SERVER_PATH")) {
        return eckit::PathName{::getenv("MULTIO_SERVER_PATH")};
    }
    return eckit::PathName{""};
}

eckit::PathName test_configuration(const std::string& type) {
    eckit::Log::debug<multio::LibMultio>() << "Transport type: " << type << std::endl;
    std::map<std::string, std::string> configs = {{"mpi", "mpi-test-config.json"},
                                                  {"tcp", "tcp-test-config.json"},
                                                  {"thread", "thread-test-config.json"},
                                                  {"none", "no-transport-test-config.json"}};

    return base() + "/configs/" + eckit::PathName{configs.at(type)};
}
}  // namespace

//----------------------------------------------------------------------------------------------------------------

class MultioReplay final : public multio::server::MultioServerTool {
public:  // methods

    MultioReplay(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    void runClient();

    void sendDomain(MultioClient& multioClient);
    void sendFields(MultioClient& multioClient);

    eckit::Buffer readGrid(const std::string& grid_type, size_t client_id);
    eckit::Buffer readField(const std::string& param, size_t client_id) const;

    bool isServer(size_t rank) const;
    size_t commSize() const;
    void testData();

    std::string transportType_ = "mpi";
    std::string pathToNemoData_ = "";

    size_t clientCount_ = 1;
    size_t serverCount_ = 1;
    size_t fieldSize_ = 1;
    size_t level_ = 1;
    size_t step_ = 24;
    std::map<std::string, std::string> parameters_ = {{"sst", "orca_grid_T"},
                                                      {"ssu", "orca_grid_U"},
                                                      {"ssv", "orca_grid_V"},
                                                      {"ssw", "orca_grid_W"}};

    eckit::LocalConfiguration config_;
    size_t rank_ = 0;
};

//----------------------------------------------------------------------------------------------------------------

MultioReplay::MultioReplay(int argc, char** argv) : multio::server::MultioServerTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("path", "Path to NEMO data"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
    options_.push_back(
        new eckit::option::SimpleOption<size_t>("field", "Name of field to replay"));
    options_.push_back(
        new eckit::option::SimpleOption<size_t>("step", "Time counter for the field to replay"));
}

void MultioReplay::init(const eckit::option::CmdArgs& args) {
    args.get("transport", transportType_);
    args.get("path", pathToNemoData_);

    args.get("nbclients", clientCount_);
    args.get("nbservers", serverCount_);

    args.get("step", step_);

    if (transportType_ != "mpi") {
        throw eckit::SeriousBug("Only MPI transport is supported for this tool");
    }

    config_ =
        eckit::LocalConfiguration{eckit::YAMLConfiguration{test_configuration(transportType_)}};

    rank_ = eckit::mpi::comm(config_.getString("group").c_str()).rank();

    auto comm_size = eckit::mpi::comm(config_.getString("group").c_str()).size();
    if (comm_size != clientCount_ + serverCount_) {
        throw eckit::SeriousBug(
            "Number of MPI ranks does not match the number of clients and servers");
    }
}

void MultioReplay::execute(const eckit::option::CmdArgs &) {
    runClient();

    testData();
 }

void MultioReplay::runClient() {

    config_.set("clientCount", clientCount_);
    config_.set("serverCount", serverCount_);

    MultioClient multioClient(config_);

    multioClient.openConnections();

    sendDomain(multioClient);

    sendFields(multioClient);

    multioClient.closeConnections();
}

void MultioReplay::sendDomain(MultioClient& multioClient) {
    for (std::string grid_type : {"grid_T", "grid_U", "grid_V", "grid_W"}) {
        auto buffer = readGrid(grid_type, rank_);

        // Send domain to each server
        auto repr = "orca_" + grid_type;
        multioClient.sendDomain(repr, "structured", std::move(buffer));
    }
}

void MultioReplay::sendFields(MultioClient& multioClient) {
    for (const auto& param : parameters_) {
        auto buffer = readField(param.first, rank_);

        Metadata metadata;
        metadata.set("igrib", param.first);
        metadata.set("ilevg", level_);
        metadata.set("istep", step_);

        multioClient.sendField(param.first, "ocean-surface", fieldSize_, param.second, metadata,
                               std::move(buffer));
    }
}

eckit::Buffer MultioReplay::readGrid(const std::string& grid_type, size_t client_id) {
    std::ostringstream oss;
    oss << grid_type << "_" << std::setfill('0') << std::setw(2) << client_id;

    auto grid = eckit::PathName{pathToNemoData_ + oss.str()};

    std::ifstream infile(std::string{grid.fullName()}.c_str());

    std::string gtype;
    infile >> gtype;
    if (gtype != grid_type) {
        throw eckit::SeriousBug("Wrong grid is being read");
    }

    std::vector<int32_t> domain_dims;
    for (int32_t next; infile >> next;) {
        domain_dims.push_back(next);
    }
    fieldSize_ = static_cast<size_t>(domain_dims[0] * domain_dims[1]);

    return eckit::Buffer{reinterpret_cast<const char*>(domain_dims.data()),
                         domain_dims.size() * sizeof(int32_t)};
}

eckit::Buffer MultioReplay::readField(const std::string& param, size_t client_id) const {
    std::ostringstream oss;
    oss << param << "_" << std::setfill('0') << std::setw(2) << step_ << "_" << std::setfill('0')
        << std::setw(2) << client_id;

    auto field = eckit::PathName{pathToNemoData_ + oss.str()};

    std::ifstream infile(std::string{field.fullName()}.c_str());
    std::string str{std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>()};

    return eckit::Buffer{str.data(), str.size()};
}

bool MultioReplay::isServer(size_t rank) const {
    return clientCount_ - 1 < rank;
}

size_t MultioReplay::commSize() const {
    return clientCount_ + serverCount_;
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
