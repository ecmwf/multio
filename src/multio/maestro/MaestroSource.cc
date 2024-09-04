
#include <fstream>
#include <thread>

#include "MaestroSource.h"

#include "eckit/io/Buffer.h"

#include "multio/maestro/ThreadsafeMap.h"
#include "multio/util/ScopedTimer.h"

namespace multio {

MaestroSource::MaestroSource(const eckit::option::CmdArgs& args) : Source(args) {}

MaestroSource::~MaestroSource() = default;

size_t MaestroSource::retrieve(const std::map<std::string, std::string>& retrieve, eckit::Buffer& field) const {
    util::ScopedTiming retrieveTiming(timer_, timing_);
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
