#include <cstring>
#include <fstream>
#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/FileHandle.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "multio/api/c/multio_c_cpp_utils.h"
#include "multio/config/PathConfiguration.h"
#include "multio/tools/MultioTool.h"


using multio::config::configuration_path_name;

namespace {
// TODO: Remove this helper class and update test configs to use the parameter-mapping action

using NemoKey = std::string;

struct GribData {
    long param;
    std::string unstructuredGridSubtype;
    std::string domain;
    std::string gridType;
    std::string levelType;
};

std::map<NemoKey, GribData> fetch_nemo_params(const eckit::Configuration& config) {
    const auto& cfgList = config.getSubConfigurations("data");
    std::map<std::string, GribData> nemo_map;
    for (auto const& cfg : cfgList) {
        nemo_map[cfg.getString("nemo-id")]
            = {cfg.getLong("param-id"), cfg.getString("unstructured-grid-subtype"), cfg.getString("domain"),
               cfg.getString("grid-type"), cfg.getString("level-type")};
    }
    return nemo_map;
}

class NemoToGrib {
public:
    NemoToGrib() :
        parameters_{fetch_nemo_params(
            eckit::YAMLConfiguration{configuration_path_name() + "metadata-mapping/nemo-to-grib.yaml"})} {}

    const GribData& get(const NemoKey& key) const { return parameters_.at(key); }

    std::map<NemoKey, GribData> parameters_;
};

}  // namespace

//----------------------------------------------------------------------------------------------------------------
/**
 * \todo Using this simple handler will throuw exceptions before any error codes can be checked. Can
 * we write separate tests which test for specific expected error codes?
 */
void rethrowMaybe(int err, multio_failure_info_t* i) {
    if (err != MULTIO_SUCCESS) {
        throw eckit::Exception("MULTIO C Exception:" + std::string{multio_error_string_info(err, i)}, Here());
    }
}

void multio_throw_failure_handler(void*, int err, multio_failure_info_t* i) {
    rethrowMaybe(err, i);
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
    void setDomains(bool onlyLoadDefinitions = false);
    void writeMasks();
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
    bool singlePrecision_;

    std::map<std::string, std::vector<int>> domainDefinitions_;

    std::string mpiGroup_ = "";
    std::string configFile_ = "";
    bool initMPIExternally_ = false;
    bool passDownMPIComm_ = false;
    bool sendMasks_ = false;

    multio_handle_t* multio_handle = nullptr;
};

//----------------------------------------------------------------------------------------------------------------

MultioReplayNemoCApi::MultioReplayNemoCApi(int argc, char** argv) :
    multio::MultioTool(argc, argv), singlePrecision_(false) {
    options_.push_back(new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("path", "Path to NEMO data"));
    options_.push_back(new eckit::option::SimpleOption<long>("step", "Time counter for the field to replay"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("mpi-group", "Name of the mpi group - default 'multio'"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("config-file", "Path to the multio config file (optional)"));
    options_.push_back(
        new eckit::option::SimpleOption<bool>("init-mpi-external",
                                              "Will use eckit::mpi to split communicators. Multio will access these "
                                              "communictors by looking up eckit named communicator map."));
    options_.push_back(new eckit::option::SimpleOption<bool>(
        "pass-down-mpi-comm", "Passdown MPI commincators as fortran INT through the API"));
    options_.push_back(new eckit::option::SimpleOption<bool>("send-masks", "Sending masks with 1 for all nodes"));

    // enable single precision testing
    for (int i = 1; i < argc; ++i) {
        if (::strcmp(argv[i], "--singlePrecision") == 0) {
            singlePrecision_ = true;
        }
    }
    return;
}

void MultioReplayNemoCApi::init(const eckit::option::CmdArgs& args) {
    args.get("transport", transportType_);
    args.get("path", pathToNemoData_);

    args.get("step", step_);

    mpiGroup_ = args.getString("mpi-group", "multio");

    if (args.has("config-file")) {
        configFile_ = args.getString("config-file");
    }

    initMPIExternally_ = args.getBool("init-mpi-external", false);
    passDownMPIComm_ = args.getBool("pass-down-mpi-comm", false);
    sendMasks_ = args.getBool("send-masks", false);

    initClient();
}

void MultioReplayNemoCApi::finish(const eckit::option::CmdArgs&) {
    multio_delete_handle(multio_handle);
}


void MultioReplayNemoCApi::execute(const eckit::option::CmdArgs&) {

    runClient();

    testData();
}

void MultioReplayNemoCApi::runClient() {
    setMetadata();

    multio_open_connections(multio_handle);

    setDomains();

    if (sendMasks_) {
        writeMasks();
    }

    writeFields();

    multio_close_connections(multio_handle);
}

void MultioReplayNemoCApi::setMetadata() {}

void MultioReplayNemoCApi::setDomains(bool onlyLoadDefinitions) {
    const std::map<std::string, std::string> grid_type
        = {{"T grid", "grid_T"}, {"U grid", "grid_U"}, {"V grid", "grid_V"}, {"W grid", "grid_W"}};

    multio_metadata_t* md = nullptr;
    if (!onlyLoadDefinitions) {
        multio_new_metadata(&md, multio_handle);

        // Global size is constant for all domains, send by parametrization (mainly for testing purpose...)
        multio_metadata_set_bool(md, "toAllServers", true);
        multio_metadata_set_int(md, "globalSize", globalSize_);
        multio_write_parametrization(multio_handle, md);
    }

    for (auto const& grid : grid_type) {
        auto buffer = readGrid(grid.second, rank_);
        auto sz = static_cast<int>(buffer.size());


        if (!onlyLoadDefinitions) {
            multio_metadata_set_string(md, "name", grid.first.c_str());

            multio_metadata_set_string(md, "category", "ocean-domain-map");
            multio_metadata_set_string(md, "representation", "structured");
            multio_metadata_set_bool(md, "toAllServers", true);

            multio_write_domain_int32(multio_handle, md, buffer.data(), sz);
        }

        domainDefinitions_.emplace(std::make_pair(grid.first, std::move(buffer)));
    }
    if (!onlyLoadDefinitions)
        multio_delete_metadata(md);
}

void MultioReplayNemoCApi::writeMasks() {
    const std::string grid_prefix[4] = {"T", "U", "V", "W"};

    for (const auto& param : parameters_) {
        const char gridPrefix = paramMap_.get(param).unstructuredGridSubtype[0];

        auto definition = domainDefinitions_.find(paramMap_.get(param).domain);
        if (definition == domainDefinitions_.end()) {
            throw eckit::SeriousBug{"No domain definitons for this gridType found", Here()};
        }

        // Create masks all set to 1, ceil the number of elemns to a multiple of 4 because mask is send as int32_t
        std::vector<float> masks(definition->second[8] * definition->second[10], 1.0);

        multio_metadata_t* md = nullptr;
        multio_new_metadata(&md, multio_handle);

        std::string name = gridPrefix + std::string(" mask");
        multio_metadata_set_string(md, "name", name.c_str());

        std::string domain = gridPrefix + std::string(" grid");
        multio_metadata_set_string(md, "domain", domain.c_str());

        multio_metadata_set_string(md, "category", "ocean-mask");
        // Global size has been set through parametrization
        // multio_metadata_set_int(md, "globalSize", globalSize_);
        multio_metadata_set_int(md, "level", level_);
        multio_metadata_set_bool(md, "toAllServers", true);

        multio_write_mask(multio_handle, md, masks.data(), masks.size());

        multio_delete_metadata(md);
    }
}


void MultioReplayNemoCApi::writeFields() {

    for (const auto& param : parameters_) {
        bool is_active = 0;
        {
            multio_metadata_t* md = nullptr;
            multio_new_metadata(&md, multio_handle);
            multio_metadata_set_string(md, "category", "ocean-2d");
            multio_field_accepted(multio_handle, md, &is_active);
            multio_delete_metadata(md);
            if (is_active) {
                throw eckit::SeriousBug{"Category should be not fully active: ocean-2d", Here()};
            }
        }

        {
            multio_metadata_t* md = nullptr;
            multio_new_metadata(&md, multio_handle);
            multio_metadata_set_string(md, "name", param.c_str());
            multio_field_accepted(multio_handle, md, &is_active);
            multio_delete_metadata(md);
            if (!is_active) {
                throw eckit::SeriousBug{"Field should be active: " + param, Here()};
            }
        }

        auto buffer = readField(param, rank_);

        auto sz = static_cast<int>(buffer.size()) / sizeof(double);
        auto fname = param.c_str();

        multio_metadata_t* md = nullptr;
        multio_new_metadata(&md, multio_handle);

        // Set reused fields once at the beginning
        multio_metadata_set_string(md, "category", "ocean-2d");
        // globalSize has been set through parametrization
        // multio_metadata_set_int(md, "globalSize", globalSize_);
        multio_metadata_set_int(md, "level", level_);
        multio_metadata_set_int(md, "step", step_);

        // To mimic nemoV4; it will be overwritten
        multio_metadata_set_double(md, "missingValue", 0.0);
        multio_metadata_set_bool(md, "bitmapPresent", false);
        multio_metadata_set_int(md, "bitsPerValue", 16);

        multio_metadata_set_bool(md, "toAllServers", false);

        // Overwrite these fields in the existing metadata object
        multio_metadata_set_string(md, "name", fname);
        multio_metadata_set_string(md, "nemoParam", fname);

        if (singlePrecision_) {
            const double* tmp_d = reinterpret_cast<const double*>(buffer.data());
            std::vector<float> tmp_f(sz, 0.0);
            for (int i = 0; i < sz; ++i) {
                tmp_f[i] = float(tmp_d[i]);
            }
            multio_write_field(multio_handle, md, tmp_f.data(), sz);
        }
        else {
            multio_write_field(multio_handle, md, reinterpret_cast<const double*>(buffer.data()), sz);
        }
        multio_delete_metadata(md);
    }
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

    infile.close();
    return buffer;
}

void MultioReplayNemoCApi::initClient() {
    if (transportType_ != "mpi") {
        throw eckit::SeriousBug("Only MPI transport is supported for this tool");
    }


    if (initMPIExternally_ || passDownMPIComm_) {
        eckit::Log::info() << " *** initializing mpi communicators before multio " << std::endl;
        eckit::mpi::addComm(mpiGroup_.c_str(), eckit::mpi::comm().communicator());
        std::string clientGroup = mpiGroup_ + "-clients";
        eckit::mpi::comm(mpiGroup_.c_str()).split(777, clientGroup.c_str());
    }


    multio_configuration_t* multio_cc = nullptr;

    if (configFile_ == "") {
        // Init through environment variable or default file
        multio_new_configuration(&multio_cc);
    }
    else {
        eckit::Log::info() << " *** multio_new_handle_from_filename " << configFile_ << std::endl;
        multio_new_configuration_from_filename(&multio_cc, configFile_.c_str());
    }

    multio_config_set_failure_handler(multio_cc, multio_throw_failure_handler, nullptr);

    int retComm = 0;
    if (passDownMPIComm_) {
        multio_mpi_parent_comm(multio_cc, eckit::mpi::comm(mpiGroup_.c_str()).communicator());
        multio_mpi_return_client_comm(multio_cc, &retComm);
    }


    multio_new_handle(&multio_handle, multio_cc);
    multio_delete_configuration(multio_cc);

    if (passDownMPIComm_) {
        eckit::Log::info() << " *** multio_new_handle mpi returned comm: " << retComm << std::endl;
        ASSERT(retComm != 0);
        ASSERT(eckit::mpi::comm("multio-clients").communicator() == retComm);
    }

    const eckit::mpi::Comm& group = eckit::mpi::comm(mpiGroup_.c_str());
    std::string clientGroup = mpiGroup_ + "-clients";
    const eckit::mpi::Comm& clients = eckit::mpi::comm(clientGroup.c_str());

    rank_ = group.rank();
    clientCount_ = clients.size();
    serverCount_ = group.size() - clientCount_;

    eckit::Log::info() << " *** initClient - clientcount:  " << clientCount_ << ", serverCount: " << serverCount_
                       << std::endl;
}

namespace {
template <typename T>
union TypeToChar {
    char c[sizeof(T)];
    T v;
};

template <typename T, typename Ind>
char getCharRepr(T v, Ind ind) {
    TypeToChar<T> ttc;
    ttc.v = v;
    return ttc.c[ind];
}
}  // namespace

void MultioReplayNemoCApi::testData() {
    eckit::mpi::comm().barrier();
    if (eckit::mpi::comm().rank() != 0) {
        return;
    }

    for (const auto& param : parameters_) {
        std::ostringstream oss;
        oss << level_ << "::" << paramMap_.get(param).param << "::" << step_;

        std::string actual_file_path{oss.str()};
        std::ifstream infile_actual{actual_file_path};

        oss.str("");
        oss.clear();
        oss << pathToNemoData_ << param << "_" << step_ << "_reference";
        auto path = eckit::PathName{oss.str()};

        std::ifstream infile_expected{path.fullName()};

        eckit::Log::info() << " *** testData - ActualFilePath: " << actual_file_path << ", expected path: " << path
                           << std::endl;


        auto begin1 = std::istreambuf_iterator<char>(infile_actual);
        auto end1 = std::istreambuf_iterator<char>();
        auto begin2 = std::istreambuf_iterator<char>(infile_expected);
        auto end2 = std::istreambuf_iterator<char>();
        bool isOutputEqual = true;
        std::size_t fidx = 0;

        for (; (begin1 != end1) && (begin2 != end2); ++begin1, ++begin2, ++fidx) {
            char b1 = *begin1;
            char b2 = *begin2;
            bool isByteEqual = (b1 == b2);

            isOutputEqual &= isByteEqual;

            // eckit::Log::info() << " *** Compare index " << fidx << ": " << std::hex << std::setiosflags
            // (std::ios::showbase) << (int) b1 << " (actual) " << (isByteEqual ? "==" : "!=") << " " << (int) b2 << "
            // (expected)" << std::resetiosflags(std::ios::hex) << std::endl;
        }
        if ((begin1 != end1) || (begin2 != end2)) {
            isOutputEqual = false;
            if (begin1 != end1) {
                eckit::Log::info() << "Expected file is shorter than actual file" << std::endl;
                ;
            }
            if (begin2 != end2) {
                eckit::Log::info() << "Actual file is shorter than expected file" << std::endl;
                ;
            }
        }
        ASSERT(isOutputEqual);

        infile_actual.close();
        infile_expected.close();
        std::remove(actual_file_path.c_str());
    }
}


//----------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioReplayNemoCApi tool(argc, argv);
    return tool.start();
}
