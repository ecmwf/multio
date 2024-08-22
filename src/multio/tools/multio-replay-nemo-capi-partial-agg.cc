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


#define XSTRM(m) STRM(m)
#define STRM(m) #m


using multio::config::configuration_file_name;
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
    // Use a value that is not showing up in the test data and that
    // can be en and decoded without changing binary layout
    // For partial aggregation test, we compare mask binary...
    //
    // Needs to be the same in the YAML files mask action...
    double missingValue = 1.234;

    size_t clientCount_ = 1;
    size_t serverCount_ = 0;
    bool singlePrecision_;

    std::map<std::string, std::vector<int>> domainDefinitions_;

    multio_handle_t* multio_handle = nullptr;
};

//----------------------------------------------------------------------------------------------------------------

MultioReplayNemoCApi::MultioReplayNemoCApi(int argc, char** argv) :
    multio::MultioTool(argc, argv), singlePrecision_(false) {
    options_.push_back(new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("path", "Path to NEMO data"));
    options_.push_back(new eckit::option::SimpleOption<long>("step", "Time counter for the field to replay"));

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

    if (eckit::mpi::comm().rank() != 0) {
        initClient();
    }
    else {
        // All clients and server split from world with 123 (defined in the yaml)
        // As we want to exclude this node, it has to split to another node
        eckit::mpi::comm().split(999, "non-multio");
    }
}

void MultioReplayNemoCApi::finish(const eckit::option::CmdArgs&) {
    if (eckit::mpi::comm().rank() != 0) {
        multio_delete_handle(multio_handle);
    }
}


void MultioReplayNemoCApi::execute(const eckit::option::CmdArgs&) {
    // For partial aggregation, client with rank 0 will send nothing
    if (eckit::mpi::comm().rank() != 0) {
        runClient();
    }
    else {
        // Load grid definitions for later testing
        setDomains(true);
    }

    testData();
}

void MultioReplayNemoCApi::runClient() {
    setMetadata();

    multio_open_connections(multio_handle);

    setDomains();

    writeMasks();

    writeFields();

    multio_close_connections(multio_handle);
}

void MultioReplayNemoCApi::setMetadata() {}

void MultioReplayNemoCApi::setDomains(bool onlyLoadDefinitions) {
    const std::map<std::string, std::string> grid_type
        = {{"T grid", "grid_T"}, {"U grid", "grid_U"}, {"V grid", "grid_V"}, {"W grid", "grid_W"}};

    multio_metadata_t* md = nullptr;
    if (!onlyLoadDefinitions)
        multio_new_metadata(&md, multio_handle);

    for (auto const& grid : grid_type) {
        auto buffer = readGrid(grid.second, rank_);
        // Compute the partial size for all multio clients
        if (eckit::mpi::comm().rank() != 0) {
            int partialSize = 0;
            eckit::mpi::comm("multio-clients").allReduce(buffer[3] * buffer[5], partialSize, eckit::mpi::sum());
            buffer.push_back(partialSize);
            eckit::Log::info() << " *** Compute partial size for " << grid.first << ": " << partialSize << "/"
                               << (buffer[0] * buffer[1]) << std::endl;
        }
        auto sz = static_cast<int>(buffer.size());


        if (!onlyLoadDefinitions) {
            multio_metadata_set_string(md, "name", grid.first.c_str());

            multio_metadata_set_string(md, "category", "ocean-domain-map");
            multio_metadata_set_string(md, "representation", "structured");
            multio_metadata_set_int(md, "globalSize", globalSize_);
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
        multio_metadata_set_int(md, "globalSize", globalSize_);
        multio_metadata_set_int(md, "level", level_);
        multio_metadata_set_bool(md, "toAllServers", true);

        multio_write_mask(multio_handle, md, masks.data(), masks.size());

        multio_delete_metadata(md);
    }
}


void MultioReplayNemoCApi::writeFields() {

    for (const auto& param : parameters_) {
        bool is_active = false;
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
        multio_metadata_set_int(md, "globalSize", globalSize_);
        multio_metadata_set_int(md, "level", level_);
        multio_metadata_set_int(md, "step", step_);

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

    multio_configuration_t* multio_cc = nullptr;
    multio_new_configuration(&multio_cc);
    multio_config_set_failure_handler(multio_cc, multio_throw_failure_handler, nullptr);
    multio_new_handle(&multio_handle, multio_cc);
    multio_delete_configuration(multio_cc);

    eckit::Log::info() << " *** DEFAULT MPI GROUP: multio " << std::endl;
    const eckit::mpi::Comm& group = eckit::mpi::comm("multio");
    const eckit::mpi::Comm& clients = eckit::mpi::comm("multio-clients");

    rank_ = eckit::mpi::comm().rank();  // Use world rank instead of group rank because we exclude one node explicitly
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

        auto definitionPair = domainDefinitions_.find(paramMap_.get(param).domain);
        if (definitionPair == domainDefinitions_.end()) {
            throw eckit::SeriousBug{"No domain definitons for this gridType found", Here()};
        }
        auto& definition = definitionPair->second;
        auto ni_global = definition[0];

        auto ibegin = definition[2];
        auto ni = definition[3];
        auto jbegin = definition[4];
        auto nj = definition[5];

        auto data_ibegin = definition[7];
        auto data_ni = definition[8];
        auto data_jbegin = definition[9];
        auto data_nj = definition[10];


        for (unsigned int i = 0; i < (singlePrecision_ ? sizeof(float) : sizeof(double)); ++i) {
            char c = singlePrecision_ ? getCharRepr(static_cast<float>(missingValue), i) : getCharRepr(missingValue, i);
            eckit::Log::info() << " *** Missing value char repr " << i << ": " << std::hex
                               << std::setiosflags(std::ios::showbase) << (int)c << std::resetiosflags(std::ios::hex)
                               << std::endl;
        }


        for (; (begin1 != end1) && (begin2 != end2); ++begin1, ++begin2, ++fidx) {
            char b1 = *begin1;
            char b2 = *begin2;
            bool isByteEqual = (b1 == b2);
            std::size_t dIndex = fidx / (singlePrecision_ ? sizeof(float) : sizeof(double));
            std::size_t iglob = dIndex % ni_global;
            std::size_t jglob = dIndex / ni_global;

            // Test if in local domain
            if (((jglob >= jbegin) && (jglob < (jbegin + nj))) && ((iglob >= ibegin) && (iglob < (ibegin + ni)))) {
                // The data has been masked, set b1 to the byte of the missing value
                // However, as the expectation data is also not masked, we can ignore these bytes in the comparison
                isByteEqual = true;

                // Test if masking has been done right
                if (singlePrecision_) {
                    ASSERT(b1 == getCharRepr(static_cast<float>(missingValue), fidx % sizeof(float)));
                }
                else {
                    ASSERT(b1 == getCharRepr(missingValue, fidx % sizeof(double)));
                }
            }

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
