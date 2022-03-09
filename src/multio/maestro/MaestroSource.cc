
#include <thread>
#include <fstream>
#include "MaestroSource.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"
#include "multio/maestro/ThreadsafeMap.h"

namespace multio {

MaestroSource::MaestroSource(const eckit::option::CmdArgs &args): Source(args) {
}

MaestroSource::~MaestroSource() {
    std::stringstream ss;
    ss << std::this_thread::get_id();
    std::ofstream logfile{std::string{"worker_" + ss.str() + "_source.log"}, std::fstream::app};
    logfile << "Source timing: " << timing_ << std::endl;
}

size_t MaestroSource::retrieve(const std::map<std::string, std::string> &retrieve, eckit::Buffer &field) const {
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
