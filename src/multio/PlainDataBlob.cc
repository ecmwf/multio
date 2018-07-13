
#include "PlainDataBlob.h"

namespace multio {
namespace test {

PlainDataBlob::PlainDataBlob(const void* data, size_t length) : DataBlob(data, length) {}
PlainDataBlob::PlainDataBlob(eckit::DataHandle& dh, size_t length) : DataBlob(dh, length) {}

const eckit::Metadata& PlainDataBlob::metadata() const {
    return metadata_;
}

void PlainDataBlob::print(std::ostream& os) const {
    os << "TestDataBlob()";
}

static eckit::DataBlobBuilder<PlainDataBlob> dbBuilder("plain");

}  // namespace test
}  // namespace multio
