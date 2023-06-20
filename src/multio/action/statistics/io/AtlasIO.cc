#include "AtlasIO.h"

#include <cstdio>
#include <cstring>
#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"


namespace multio::action {

namespace {

template <typename T>
class SubVector {
private:
    T* data_;
    bool good_;
    size_t size_;

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
void interprete(const SubVector<T>& in, atlas::io::ArrayReference& out) {
    out = atlas::io::ArrayReference(in.data(), atlas::io::make_datatype<T>(), atlas::io::ArrayShape{in.size()});
}

template <typename T>
void decode(const atlas::io::Metadata& metadata, const atlas::io::Data& data, SubVector<T>& out) {
    atlas::io::ArrayMetadata array(metadata);
    if (out.checkSize(array.shape(0))) {
        ::memcpy(out.data(), data, data.size());
    }
    else {
        out.setStatus(false);
    }
}
}  // namespace

AtlasIO::AtlasIO(const std::string& path, const std::string& prefix) : StatisticsIO{path, prefix, "atlasIO"} {};

void AtlasIO::write(const std::string& name, std::size_t writeSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the window write file is :: " << generateCurrFileName(name)
                             << std::endl;
    removeCurrFile(name);
    const std::string fname = generateCurrFileName(name);
    atlas::io::RecordWriter record;
    SubVector<std::uint64_t> dat{buffer_.data(), writeSize};
    record.set(name, atlas::io::ref(dat), no_compression);
    record.write(fname);
    removePrevFile(name);
    return;
};

void AtlasIO::read(const std::string& name, std::size_t readSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateCurrFileName(name)
                             << std::endl;
    const std::string fname = generateCurrFileName(name);
    checkFileExist(fname);
    atlas::io::RecordReader record(fname);
    SubVector<std::uint64_t> dat{buffer_.data(), readSize};
    record.read(name, dat).wait();
    if (!dat.good()) {
        std::ostringstream os;
        os << "ERROR : wrong file size for restart : (" << name << ")";
        throw eckit::SeriousBug{os.str(), Here()};
    }
    return;
};

void AtlasIO::flush() {
    // TODO: Decide what to do when flush is called. Flush partial statistics when the Tag::Flush is received is
    // probably okay
    return;
};

void AtlasIO::checkFileExist(const std::string& name) const {
    eckit::PathName file{name};
    if (!file.exists()) {
        std::ostringstream os;
        os << "ERROR : wrong file not exist : (" << name << ")";
        throw eckit::SeriousBug{os.str(), Here()};
    }
    return;
};

StatisticsIOBuilder<AtlasIO> AtalsIOBuilder("atlas_io");

}  // namespace multio::action
