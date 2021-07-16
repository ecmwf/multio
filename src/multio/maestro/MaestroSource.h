
#ifndef multio_MaestroSource_H
#define multio_MaestroSource_H

#include "pgen/sources/Source.h"
extern "C" {
#include <maestro.h>
}

namespace multio {

class MaestroSource : pgen::Source {
public:
    MaestroSource(const eckit::option::CmdArgs &args);
private:
    size_t retrieve(const std::map<std::string, std::string> &retrieve, eckit::Buffer &field) const override;
    void print(std::ostream &out) const override;
    friend class MaestroSyphon;
};

}  // namespace multio

#endif  // multio_MaestroSource_H
