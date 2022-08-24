
#include <fstream>
#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/io/FileHandle.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/log/JSON.h"
#include "eckit/config/YAMLConfiguration.h"


#include "multio/LibMultio.h"
// #include "multio/server/MultioNemo.h"
#include "multio/api/multio_c.h"
#include "multio/server/NemoToGrib.h"
#include "multio/tools/MultioTool.h"
#include "multio/util/ConfigurationPath.h"

#include "multio/message/Metadata.h"



using multio::util::configuration_file;

//----------------------------------------------------------------------------------------------------------------
/**
 * \todo Using this simple handler will throuw exceptions before any error codes can be checked. Can we write separate tests which test for specific expected error codes?
 */
void rethrowMaybe(int err) {
    if (err != MULTIO_SUCCESS) {
        throw eckit::Exception{"MULTIO C Exception:" + std::string{multio_error_string(err)}};
    }
}

void multio_throw_failure_handler(void*, int err) {
    rethrowMaybe(err);
}

class MultioReplayNemoCApi final : public multio::MultioTool {
public:

    MultioReplayNemoCApi(int argc, char** argv);

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
    eckit::Buffer readField(const std::string& param, size_t client_id) const;

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

    size_t clientCount_ = 1;
    size_t serverCount_ = 0;

    multio_handle_t* multio_handle = nullptr;

    eckit::PathName configPath_;
    eckit::LocalConfiguration config_;
};

//----------------------------------------------------------------------------------------------------------------

MultioReplayNemoCApi::MultioReplayNemoCApi(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("path", "Path to NEMO data"));
    options_.push_back(new eckit::option::SimpleOption<long>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<long>("field", "Name of field to replay"));
    options_.push_back(
        new eckit::option::SimpleOption<long>("step", "Time counter for the field to replay"));
}

void MultioReplayNemoCApi::init(const eckit::option::CmdArgs& args) {
    args.get("transport", transportType_);
    args.get("path", pathToNemoData_);

    args.get("step", step_);

    initClient();
}

void MultioReplayNemoCApi::finish(const eckit::option::CmdArgs&) {
    multio_delete_handle(multio_handle);
}

void MultioReplayNemoCApi::execute(const eckit::option::CmdArgs &) {
    runClient();

    testData();
 }

 void MultioReplayNemoCApi::runClient() {
     setMetadata();

     multio_open_connections(multio_handle);

     setDomains();

     writeFields();

     multio_close_connections(multio_handle);
 }

void MultioReplayNemoCApi::setMetadata() {
}

void MultioReplayNemoCApi::setDomains() {
    const std::map<std::string, std::string> grid_type = {
        {"T grid", "grid_T"}, {"U grid", "grid_U"}, {"V grid", "grid_V"}, {"W grid", "grid_W"}};

        multio_metadata_t* md = nullptr; 
    multio_new_metadata(&md);

    for (auto const& grid : grid_type) {
        auto buffer = readGrid(grid.second, rank_);
        auto sz = static_cast<int>(buffer.size());
        multio_metadata_set_string_value(md, "name", grid.first.c_str());

        multio_metadata_set_string_value(md, "category", "ocean-domain-map");
        multio_metadata_set_string_value(md, "representation", "structured");
        //! How to determine number of clients with API?
        multio_metadata_set_int_value(md, "domainCount", clientCount_);
        multio_metadata_set_bool_value(md, "toAllServers", true);

        multio_write_domain(multio_handle, md, buffer.data(), sz);
    }
    multio_delete_metadata(md);
}

void MultioReplayNemoCApi::writeFields() {
    multio_metadata_t* md = nullptr; 
    multio_new_metadata(&md);

    // multio_metadata_t* runConfig;
    // multio_new_metadata_from_yaml(&runConfig, multio::message::to_string(multio::message::Metadata(config_.getSubConfiguration("run"))).c_str());
     
    // TODO: Actually not required to pass this runconfig
    // multio_metadata_set_map_value(md, "run", runConfig);
    // TODO: These fields are also not required? Test passes also without
    // Set reused fields once at the beginning
    multio_metadata_set_string_value(md, "category", "ocean-2d");
    multio_metadata_set_int_value(md, "globalSize", globalSize_);
    multio_metadata_set_int_value(md, "level", level_);
    multio_metadata_set_int_value(md, "step", step_);

    // TODO: May not need to be a field's metadata
    multio_metadata_set_double_value(md, "missingValue", 0.0);
    multio_metadata_set_bool_value(md, "bitmapPresent", false);
    multio_metadata_set_int_value(md, "bitsPerValue", 16);

    multio_metadata_set_bool_value(md, "toAllServers", false);

    for (const auto& param : parameters_) {
        auto buffer = readField(param, rank_);

        auto sz = static_cast<int>(buffer.size())/sizeof(double);
        auto fname = param.c_str(); 

        // Overwrite these fields in the existing metadata object
        multio_metadata_set_string_value(md, "name", fname);
        multio_metadata_set_string_value(md, "nemoParam", fname);
        multio_metadata_set_int_value(md, "param", paramMap_.get(fname).param);
        multio_metadata_set_string_value(md, "gridSubtype", paramMap_.get(fname).gridType.c_str());
        multio_metadata_set_int_value(md, "domainCount", clientCount_);
        multio_metadata_set_string_value(md, "domain", paramMap_.get(fname).gridType.c_str());
        multio_metadata_set_string_value(md, "typeOfLevel", paramMap_.get(fname).levelType.c_str());

        multio_write_field(multio_handle, md, reinterpret_cast<const double*>(buffer.data()), sz);
    }
    multio_delete_metadata(md);
    // multio_delete_metadata(runConfig);
}

std::vector<int> MultioReplayNemoCApi::readGrid(const std::string& grid_type, size_t client_id) {
    std::ostringstream oss;
    oss << pathToNemoData_ << grid_type << "_" << std::setfill('0') << std::setw(2) << client_id;

    auto grid = eckit::PathName{oss.str()};

    std::ifstream infile(grid.fullName());

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

eckit::Buffer MultioReplayNemoCApi::readField(const std::string& param, size_t client_id) const {
    std::ostringstream oss;
    oss << pathToNemoData_ << param << "_" << std::setfill('0') << std::setw(2) << step_ << "_" << std::setfill('0')
        << std::setw(2) << client_id;

    auto field = eckit::PathName{oss.str()};

    eckit::Log::info() << " *** Reading path " << field << std::endl;

    eckit::FileHandle infile{field.fullName()};
    size_t bytes = infile.openForRead();

    eckit::Buffer buffer(bytes);
    infile.read(buffer.data(), bytes);

    return buffer;
}

// std::vector<double> MultioReplayNemoCApi::readField(const std::string& param, size_t client_id) const {
//     std::ostringstream oss;
//     oss << param << "_" << std::setfill('0') << std::setw(2) << step_ << "_" << std::setfill('0')
//         << std::setw(2) << client_id;

//     auto field = eckit::PathName{pathToNemoData_ + oss.str()};

//     eckit::Log::info() << " *** Reading path " << field << std::endl;

//     std::ifstream infile(field.fullName());
//     infile.seekg(0, infile.end);
//     auto bytes = infile.tellg();
//     infile.seekg(0, infile.beg);

//     std::vector<double> vals(bytes / sizeof(double));
//     infile.read(reinterpret_cast<char*>(vals.data()), bytes);

//     return vals;
// }

void MultioReplayNemoCApi::initClient() {
    if (transportType_ != "mpi") {
        throw eckit::SeriousBug("Only MPI transport is supported for this tool");
    }
    //! TODO run metadata will need to be fetched from config. Hence config is read twice - in the api and here
    configPath_ = configuration_file();
    config_ = eckit::LocalConfiguration{eckit::YAMLConfiguration{configPath_}};

    rank_ = eckit::mpi::comm("world").rank();
    eckit::mpi::addComm("nemo", eckit::mpi::comm().communicator());

    auto configPath = configuration_file();

    multio_set_failure_handler(multio_throw_failure_handler, nullptr);
    multio_new_handle_from_config(&multio_handle, configPath.asString().c_str());
    //! Not required in new transport based api?
    // multio_init_client("oce", eckit::mpi::comm().communicator());
    // Simplate client that knows about nemo communicator and oce
    // eckit::mpi::addComm("nemo", eckit::mpi::comm().communicator());

    // TODO: find a way to come up with a unique 'colour' -- like getting application number
    const eckit::mpi::Comm& chld = eckit::mpi::comm("nemo").split(777, "oce");
        auto ret_comm = chld.communicator();

    // Access information directly with mpi
    // TODO: Provide different API mechanisms to do this?
    clientCount_ = eckit::mpi::comm("oce").size();
    serverCount_ = eckit::mpi::comm("nemo").size() - clientCount_;

    eckit::Log::info() << " *** initClient - clientcount:  " << clientCount_ << ", serverCount: " << serverCount_ << std::endl;

}

void MultioReplayNemoCApi::testData() {
    eckit::mpi::comm().barrier();
    if (eckit::mpi::comm().rank() != 0) {
        return;
    }

    for (const auto& param : parameters_) {
        std::ostringstream oss;
        oss << level_ << "::" << paramMap_.get(param).param << "::" << step_;

        std::string actual_file_path{oss.str()};
        std::ifstream infile{actual_file_path.c_str()};
        std::string actual{std::istreambuf_iterator<char>(infile),
                           std::istreambuf_iterator<char>()};
        infile.close();

        oss.str("");
        oss.clear();
        oss << pathToNemoData_ << param << "_" << step_ << "_reference";
        auto path = eckit::PathName{oss.str()};

        infile.open(std::string(path.fullName()).c_str());
        std::string expected{std::istreambuf_iterator<char>(infile),
                             std::istreambuf_iterator<char>()};

        eckit::Log::info() << " *** testData - ActualFilePath: " << actual_file_path << ", expected path: " << path << std::endl;

        ASSERT(actual == expected);

        std::remove(actual_file_path.c_str());
    }
}


//----------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioReplayNemoCApi tool(argc, argv);
    return tool.start();
}
