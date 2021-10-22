
#include "MaestroSource.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"
#include "multio/maestro/ThreadsafeMap.h"

namespace multio {

MaestroSource::MaestroSource(const eckit::option::CmdArgs &args, MaestroStatistics& statistics):
    Source(args), statistics_{statistics} {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.sourceConstructionTiming_);
}

size_t MaestroSource::retrieve(const std::map<std::string, std::string> &retrieve, eckit::Buffer &field) const {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.sourceRetrieveTiming_);
    auto cdo_name = cdo_namer_.name(retrieve);
    MaestroCdo cdo = CdoMap::instance().get(cdo_name);
    cdo.demand();
    field = eckit::Buffer{cdo.data(), cdo.size()};
    cdo.dispose();
    CdoMap::instance().erase(cdo_name);
    return field.size();
}

void MaestroSource::print(std::ostream& out) const {
    out << "MaestroSource[]";
}

}  // namespace multio
