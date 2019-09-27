
#include "PlainDataBlob.h"

#include "eckit/io/DataHandle.h"
#include "eckit/log/Bytes.h"

namespace multio {
namespace server {

PlainDataBlob::PlainDataBlob(const void *data, size_t length)
    : DataBlob(data, length) {}
PlainDataBlob::PlainDataBlob(eckit::DataHandle &dh, size_t length)
    : DataBlob(dh, length) {}

const eckit::Metadata &PlainDataBlob::metadata() const { return metadata_; }

void PlainDataBlob::print(std::ostream &os) const {
  os << "PlainDataBlob[size=" << eckit::Bytes(buffer_.size())
     << ", metadata=" << metadata_ << "]";
}

static eckit::DataBlobBuilder<PlainDataBlob> plainDataBlobBuilder("plain");

}  // namespace server
}  // namespace multio
