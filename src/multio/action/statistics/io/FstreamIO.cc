#include "FstreamIO.h"

#include <cstdio>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"

namespace multio::action::statistics {

FstreamIO::FstreamIO(const std::string& path, const std::string& prefix) : StatisticsIO{path, prefix, "fstreamIO"} {};

void FstreamIO::write(const std::string& name, std::size_t fieldSize, std::size_t writeSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the window write file is :: " << generateCurrFileName(name)
                             << std::endl;
    const std::string fname = generateCurrFileName(name);
    std::FILE* fp = std::fopen(fname.c_str(), "w");
    std::uint64_t sz = static_cast<std::uint64_t>(fieldSize);
    std::fwrite(&sz, sizeof(std::uint64_t), 1, fp);
    std::fwrite(buffer_.data(), sizeof(std::uint64_t), writeSize, fp);
    std::fflush(fp);
    std::fclose(fp);
    return;
};

void FstreamIO::readSize(const std::string& name, std::size_t& readSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateCurrFileName(name)
                             << std::endl;
    const std::string fname = generateCurrFileName(name);
    checkFileExist(fname);
    std::uint64_t sz;
    std::FILE* fp = std::fopen(fname.c_str(), "r");
    std::fread(&sz, sizeof(uint64_t), 1, fp);
    readSize = static_cast<std::size_t>(sz);
    std::fclose(fp);
    return;
};

void FstreamIO::read(const std::string& name, std::size_t readSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateCurrFileName(name)
                             << std::endl;
    const std::string fname = generateCurrFileName(name);
    checkFileExist(fname);
    checkFileSize(fname, readSize * sizeof(std::uint64_t));
    std::FILE* fp = std::fopen(fname.c_str(), "r");
    std::uint64_t sz;
    std::fread(&sz, sizeof(uint64_t), 1, fp);
    std::fread(buffer_.data(), sizeof(uint64_t), readSize, fp);
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

}  // namespace multio::action::statistics
