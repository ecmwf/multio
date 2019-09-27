
#ifndef multio_PlainDataBlob_H
#define multio_PlainDataBlob_H

#include <iostream>
#include <string>
#include <vector>

#include "eckit/io/DataBlob.h"
#include "eckit/types/Metadata.h"


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
    PlainDataBlob(const void* data, size_t length);
    PlainDataBlob(eckit::DataHandle& dh, size_t length);

    const eckit::Metadata& metadata() const override;

private:  // methods
    void print(std::ostream& os) const override;

private:  // members
    PlainMetadata metadata_;
};

}  // namespace server
}  // namespace multio

#endif
