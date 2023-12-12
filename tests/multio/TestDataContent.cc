
#include "TestDataContent.h"

#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/MemoryHandle.h"

namespace multio {
namespace test {

TestDataContent::TestDataContent(const void* data, size_t size) : data_{data}, size_{size} {}

TestDataContent::TestDataContent(const void* data, size_t size, const message::Metadata& md) :
    data_{data}, size_{size}, metadata_{md} {}

void TestDataContent::write(eckit::DataHandle& handle) const {
    if (handle.write(data_, size_) != static_cast<long>(size_)) {
        std::ostringstream oss;
        oss << "Write error to data handle " << handle;
        throw eckit::WriteError{oss.str(), Here()};
    }
}

eckit::DataHandle* TestDataContent::readHandle() const {
    return new eckit::MemoryHandle{data_, size_};
}

const void* TestDataContent::data() const {
    return data_;
}

size_t TestDataContent::length() const {
    return size_;
}

void TestDataContent::print(std::ostream& os) const {
    os << "multio::message::UserContent";
}

std::string TestDataContent::getString(const std::string& key) const {
    return metadata_.getString(key);
}

long TestDataContent::getLong(const std::string& key) const {
    return metadata_.getLong(key);
}

double TestDataContent::getDouble(const std::string& key) const {
    return metadata_.getDouble(key);
}

}  // namespace test
}  // namespace multio
