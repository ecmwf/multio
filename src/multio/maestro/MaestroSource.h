
#ifndef multio_MaestroSource_H
#define multio_MaestroSource_H

extern "C" {
#include <maestro.h>
}

#include "eckit/log/Statistics.h"
#include "multio/maestro/CdoNamer.h"
#include "pgen/sources/Source.h"
namespace multio {

class MaestroSource : pgen::Source {
public:
    MaestroSource(const eckit::option::CmdArgs &args);
    ~MaestroSource();
private:
    size_t retrieve(const std::map<std::string, std::string> &retrieve, eckit::Buffer &field) const override;
    void print(std::ostream &out) const override;
    CdoNamer cdo_namer_;
    mutable eckit::Timing timing_;
    mutable eckit::Timer timer_;
    friend class MaestroWorker;
};

}  // namespace multio

#endif  // multio_MaestroSource_H
