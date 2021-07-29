

#include "MaestroSource.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"
#include "multio/maestro/ThreadsafeMap.h"

namespace multio {

MaestroSource::MaestroSource(const eckit::option::CmdArgs &args):
    Source(args) {
}

size_t MaestroSource::retrieve(const std::map<std::string, std::string> &retrieve, eckit::Buffer &field) const {
    auto cdo_name = cdo_namer_.name(retrieve);
    auto&cdo = CdoMap::instance().at(cdo_name);
    cdo.demand();
    field = std::move(eckit::Buffer{cdo.data(), cdo.size()});
    CdoMap::instance().erase(cdo_name);
    return cdo.size();
}

void MaestroSource::print(std::ostream& out) const {
    out << "MaestroSource[]";
}

}  // namespace multio
