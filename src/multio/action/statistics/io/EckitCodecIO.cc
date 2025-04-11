
#include "EckitCodecIO.h"

#include <cstdio>
#include <cstring>
#include <iomanip>

#include "eckit/codec/codec.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "multio/LibMultio.h"


namespace multio::action::statistics {

namespace {

template <typename T>
class SubVector {
private:
    T* data_;
    size_t size_;
    bool good_;

public:
    size_t size() const { return size_; }
    T* data() { return data_; }
    const T* data() const { return data_; }
    SubVector(T* vec, size_t size) : data_{vec}, size_{size}, good_{true} {};
    bool checkSize(size_t size) { return size == size_; };
    void setStatus(bool stat) { good_ = false; };
    bool good() { return good_; };
};

template <typename T>
void interprete(const SubVector<T>& in, eckit::codec::ArrayReference& out) {
    out = eckit::codec::ArrayReference(in.data(), eckit::codec::make_datatype<T>(), eckit::codec::ArrayShape{in.size()});
}

template <typename T>
void decode(const eckit::codec::Metadata& metadata, const eckit::codec::Data& data, SubVector<T>& out) {
    eckit::codec::ArrayMetadata array(metadata);
    if (out.checkSize(array.shape(0))) {
        ::memcpy(out.data(), data, data.size());
    }
    else {
        out.setStatus(false);
    }
}
}  // namespace

EckitCodecIO::EckitCodecIO(const std::string& path, const std::string& prefix) : StatisticsIO{path, prefix, "atlasIO"} {};

void EckitCodecIO::write(const std::string& name, std::size_t fieldSize, std::size_t writeSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the window write file is :: " << generateCurrFileName(name)
                             << std::endl;
    const std::string fname = generateCurrFileName(name);
    eckit::codec::RecordWriter record;
    SubVector<std::uint64_t> dat{buffer_.data(), writeSize};
    record.set("size", fieldSize, no_compression);
    record.set(name, eckit::codec::ref(dat), no_compression);
    record.write(fname);
};

void EckitCodecIO::readSize(const std::string& name, std::size_t& readSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateCurrFileName(name)
                             << std::endl;
    const std::string fname = generateCurrFileName(name);
    checkFileExist(fname);
    std::uint64_t sz;
    eckit::codec::RecordReader record(fname);
    record.read("size", sz).wait();
    readSize = static_cast<std::size_t>(sz);
};

void EckitCodecIO::read(const std::string& name, std::size_t readSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateCurrFileName(name)
                             << std::endl;
    const std::string fname = generateCurrFileName(name);
    checkFileExist(fname);
    eckit::codec::RecordReader record(fname);
    SubVector<std::uint64_t> dat{buffer_.data(), readSize};
    record.read(name, dat).wait();
    if (!dat.good()) {
        std::ostringstream os;
        os << "ERROR : wrong file size for restart : (" << name << ")";
        throw eckit::SeriousBug{os.str(), Here()};
    }
};

void EckitCodecIO::flush() {
    // TODO: Decide what to do when flush is called. Flush partial statistics when the Tag::Flush is received is
    // probably okay
};

void EckitCodecIO::checkFileExist(const std::string& name) const {
    eckit::PathName file{name};
    if (!file.exists()) {
        std::ostringstream os;
        os << "ERROR : wrong file not exist : (" << name << ")";
        throw eckit::SeriousBug{os.str(), Here()};
    }
};

StatisticsIOBuilder<EckitCodecIO> EckitCodecIOBuilder("eckit_codec");
StatisticsIOBuilder<EckitCodecIO> AtlasIOBuilder("atlas_io");  // Legacy name

}  // namespace multio::action::statistics
