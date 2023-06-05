#include "FstreamIO.h"

#include <cstdio>
#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"

namespace multio::action {

FstreamIO::FstreamIO(const std::string& path, const std::string& prefix, long step) :
    StatisticsIO{path, prefix, step} {};

void FstreamIO::writePeriod(const std::string& name, const std::array<std::uint64_t, 15>& data) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the window write file is :: " << generateFileName(name, 0) << std::endl;
    const std::string fname = generateFileName(name, 0);
    std::FILE* fp = std::fopen(fname.c_str(), "w");
    std::fwrite(data.data(), sizeof(std::uint64_t), data.size(), fp);
    std::fflush(fp);
    std::fclose(fp);
    removeOldFile(name, 2);
    return;
};

void FstreamIO::readPeriod(const std::string& name, std::array<std::uint64_t, 15>& data) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateFileName(name, 0)
                             << std::endl;
    const std::string fname = generateFileName(name, 0);
    checkFileExist(fname);
    checkFileSize(fname, data.size() * sizeof(std::uint64_t));
    std::FILE* fp = std::fopen(fname.c_str(), "r");
    std::fread(data.data(), sizeof(double), data.size(), fp);
    std::fclose(fp);
    return;
};

void FstreamIO::writeOperation(const std::string& name, const std::vector<double>& data) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation write file is :: " << generateFileName(name, 0)
                             << std::endl;
    const std::string fname = generateFileName(name, 0);
    std::FILE* fp = std::fopen(fname.c_str(), "w");
    std::fwrite(data.data(), sizeof(double), data.size(), fp);
    std::fflush(fp);
    std::fclose(fp);
    removeOldFile(name, 2);
    return;
};

void FstreamIO::readOperation(const std::string& name, std::vector<double>& data) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateFileName(name, 0)
                             << std::endl;
    const std::string fname = generateFileName(name, 0);
    checkFileExist(fname);
    checkFileSize(fname, data.size() * sizeof(double));
    std::FILE* fp = std::fopen(fname.c_str(), "r");
    std::fread(data.data(), sizeof(double), data.size(), fp);
    std::fclose(fp);
    return;
};

void FstreamIO::flush() {
    return;
};

const std::string FstreamIO::generateFileName(const std::string& name, long step_offset) const {
    std::ostringstream os;
    os << path_ << "/" << prefix_ << "-" << key_ << "-" << std::setw(10) << std::setfill('0') << step_ - step_offset
       << "-" << suffix_ << "-" << name << ".bin";
    return os.str();
};


void FstreamIO::checkFileExist(const std::string& name) const {
    eckit::PathName file{name};
    if (!file.exists()) {
        std::ostringstream os;
        os << "ERROR : wrong file not exist : (" << name << ")";
        throw eckit::SeriousBug{os.str(), Here()};
    }
    return;
};

void FstreamIO::checkFileSize(const std::string& name, size_t expectedSize) const {
    eckit::PathName file{name};
    if (file.size() != expectedSize) {
        std::ostringstream os;
        os << "ERROR : wrong file size for restart : (" << name << ")";
        throw eckit::SeriousBug{os.str(), Here()};
    }
    return;
};

void FstreamIO::removeOldFile(const std::string& name, long step_offset) const {
    eckit::PathName file{generateFileName(name, step_offset)};

    if (file.exists()) {
        file.unlink();
    }
};

StatisticsIOBuilder<FstreamIO> FstreamBuilder("fstream_io");

}  // namespace multio::action