
#include "DataContent.h"

#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/MemoryHandle.h"

namespace multio {
namespace message {

DataContent::DataContent(const void* data, size_t size) : data_{data}, size_{size} {}

DataContent::DataContent(const void* data, size_t size, const Metadata& metadata) :
    data_{data}, size_{size}, metadata_{metadata} {}

eckit::DataHandle* DataContent::readHandle() const {
    return new eckit::MemoryHandle{data_, size_};
}

size_t DataContent::length() const {
    return size_;
}

void DataContent::write(eckit::DataHandle& handle) const {
    if (handle.write(data_, size_) != static_cast<long>(size_)) {
        std::ostringstream oss;
        oss << "Write error to data handle " << handle;
        throw eckit::WriteError{oss.str(), Here()};
    }
}

void DataContent::print(std::ostream& os) const {
    os << "multio::message::DataContent";
}

std::string DataContent::getString(const std::string& key) const {
    return metadata_.getString(key);
}

long DataContent::getLong(const std::string &key) const {
    return metadata_.getLong(key);
}

double DataContent::getDouble(const std::string &key) const {
    return metadata_.getDouble(key);
}

}  // namespace message
}  // namespace multio
