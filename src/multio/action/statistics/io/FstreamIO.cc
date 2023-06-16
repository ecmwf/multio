#include "FstreamIO.h"

#include <cstdio>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"

namespace multio::action {

FstreamIO::FstreamIO(const std::string& path, const std::string& prefix) : StatisticsIO{path, prefix, "fstreamIO"} {};

void FstreamIO::write(const std::string& name, const std::vector<std::uint64_t>& data) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the window write file is :: " << generateFileName(name, 0) << std::endl;
    removeOldFile(name, 0);
    const std::string fname = generateFileName(name, 0);
    std::FILE* fp = std::fopen(fname.c_str(), "w");
    std::fwrite(data.data(), sizeof(std::uint64_t), data.size(), fp);
    std::fflush(fp);
    std::fclose(fp);
    removeOldFile(name, 2);
    return;
};

void FstreamIO::read(const std::string& name, std::vector<std::uint64_t>& data) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateFileName(name, 0)
                             << std::endl;
    const std::string fname = generateFileName(name, 0);
    checkFileExist(fname);
    checkFileSize(fname, data.size() * sizeof(std::uint64_t));
    std::FILE* fp = std::fopen(fname.c_str(), "r");
    std::fread(data.data(), sizeof(uint64_t), data.size(), fp);
    std::fclose(fp);
    return;
};

void FstreamIO::flush() {
    // TODO: Decide what to do when flush is called. Flush partial statistics when the Tag::Flush is received is
    // probably okay
    return;
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

StatisticsIOBuilder<FstreamIO> FstreamBuilder("fstream_io");

}  // namespace multio::action
