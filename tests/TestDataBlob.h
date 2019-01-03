
#ifndef multio_TestDataBlob_H
#define multio_TestDataBlob_H

#include "eckit/io/DataBlob.h"
#include "eckit/io/DataHandle.h"
#include "eckit/types/Metadata.h"

#include <iostream>
#include <string>
#include <vector>

namespace multio {
namespace test {

// A trivial do-nothing metadata object
class TestMetadata final : public eckit::Metadata {
public:  // methods
    TestMetadata() {}
    std::vector<std::string> keywords() const override { NOTIMP; };
    bool has(const std::string& name) const override { NOTIMP; }
    void get(const std::string& name, std::string& value) const override { NOTIMP; }
    void get(const std::string& name, long& value) const override { NOTIMP; }
    void get(const std::string& name, double& value) const override { NOTIMP; }
    friend std::ostream& operator<<(std::ostream& s, const TestMetadata& p) {
        p.print(s);
        return s;
    }

protected:  // methods
    void print(std::ostream& os) const override { os << "TestMetadata()"; }
};

// A null datablob for testing the factories
class TestDataBlob final : public eckit::DataBlob {
public:  // methods
    TestDataBlob(const void* data, size_t length) : DataBlob(data, length) {}
    TestDataBlob(eckit::DataHandle& dh, size_t length) : DataBlob(dh, length) {}

    const eckit::Metadata& metadata() const override { return metadata_; }

private:  // methods
    void print(std::ostream& os) const override { os << "TestDataBlob()"; }

private:  // members
    TestMetadata metadata_;
};

static eckit::DataBlobBuilder<TestDataBlob> dbBuilder("test");

}  // namespace test
}  // namespace multio

#endif
