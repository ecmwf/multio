
#include "UserContent.h"

#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/MemoryHandle.h"

namespace multio {
namespace message {

UserContent::UserContent(const void* data, size_t size) : data_{data}, size_{size} {}

UserContent::UserContent(const void* data, size_t size, const Metadata& metadata) :
    data_{data}, size_{size}, metadata_{metadata} {}

void UserContent::write(eckit::DataHandle& handle) const {
    if (handle.write(data_, size_) != static_cast<long>(size_)) {
        std::ostringstream oss;
        oss << "Write error to data handle " << handle;
        throw eckit::WriteError{oss.str(), Here()};
    }
}

eckit::DataHandle* UserContent::readHandle() const {
    return new eckit::MemoryHandle{data_, size_};
}

const void* UserContent::data() const {
    return data_;
}

size_t UserContent::length() const {
    return size_;
}

void UserContent::print(std::ostream& os) const {
    os << "multio::message::UserContent";
}

std::string UserContent::getString(const std::string& key) const {
    return metadata_.getString(key);
}

long UserContent::getLong(const std::string &key) const {
    return metadata_.getLong(key);
}

double UserContent::getDouble(const std::string &key) const {
    return metadata_.getDouble(key);
}

}  // namespace message
}  // namespace multio
