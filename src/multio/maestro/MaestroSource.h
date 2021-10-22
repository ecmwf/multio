
#ifndef multio_MaestroSource_H
#define multio_MaestroSource_H

extern "C" {
#include <maestro.h>
}

#include "multio/maestro/CdoNamer.h"
#include "pgen/sources/Source.h"
namespace multio {


class MaestroSource : pgen::Source {
public:
    MaestroSource(const eckit::option::CmdArgs &args);
private:
    size_t retrieve(const std::map<std::string, std::string> &retrieve, eckit::Buffer &field) const override;
    void print(std::ostream &out) const override;
    CdoNamer cdo_namer_;
    friend class MaestroWorker;
};

}  // namespace multio

#endif  // multio_MaestroSource_H
