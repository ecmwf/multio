
#ifndef multio_PlainDataBlob_H
#define multio_PlainDataBlob_H

#include "eckit/io/DataBlob.h"
#include "eckit/io/DataHandle.h"
#include "eckit/log/Bytes.h"
#include "eckit/types/Metadata.h"

#include <iostream>
#include <string>
#include <vector>

namespace multio {
namespace server {

// A trivial do-nothing metadata object
class PlainMetadata final : public eckit::Metadata {
public:  // methods
    PlainMetadata() {}
    std::vector<std::string> keywords() const override { return {}; }
    bool has(const std::string& ) const override { return true; }
    void get(const std::string& , std::string& ) const override { }
    void get(const std::string& , long& ) const override { }
    void get(const std::string& , double& ) const override { }
    friend std::ostream& operator<<(std::ostream& s, const PlainMetadata& p) {
        p.print(s);
        return s;
    }

protected:  // methods
    void print(std::ostream& os) const override { os << "PlainMetadata()"; }
};

// A null datablob for testing the factories
class PlainDataBlob final : public eckit::DataBlob {
public:  // methods
    PlainDataBlob(const void* data, size_t length) : DataBlob(data, length) {}
    PlainDataBlob(eckit::DataHandle& dh, size_t length) : DataBlob(dh, length) {}

    const eckit::Metadata& metadata() const override { return metadata_; }

private:  // methods
    void print(std::ostream& os) const override {
        os << "PlainDataBlob[size=" << eckit::Bytes(buffer_.size()) << ", metadata=" << metadata_
           << "]";
    }

private:  // members
    PlainMetadata metadata_;
};

static eckit::DataBlobBuilder<PlainDataBlob> plainDataBlobBuilder("plain");

}  // namespace server
}  // namespace multio

#endif
