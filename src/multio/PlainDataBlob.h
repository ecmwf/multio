
#pragma once

#include "eckit/io/DataBlob.h"
#include "eckit/io/DataHandle.h"
#include "eckit/types/Metadata.h"

#include <iostream>
#include <string>
#include <vector>

namespace multio {
namespace test {

// A trivial do-nothing metadata object
class PlainMetadata final : public eckit::Metadata {
public:  // methods
    PlainMetadata() {}
    std::vector<std::string> keywords() const override { NOTIMP; };
    bool has(const std::string& name) const override { NOTIMP; }
    void get(const std::string& name, std::string& value) const override { NOTIMP; }
    void get(const std::string& name, long& value) const override { NOTIMP; }
    void get(const std::string& name, double& value) const override { NOTIMP; }
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
    PlainDataBlob(const void* data, size_t length);
    PlainDataBlob(eckit::DataHandle& dh, size_t length);

    const eckit::Metadata& metadata() const override;

private:  // methods
    void print(std::ostream& os) const override;

private:  // members
    PlainMetadata metadata_;
};

}  // namespace test
}  // namespace multio
