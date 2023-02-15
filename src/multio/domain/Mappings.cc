
#include "Mappings.h"

#include <cstring>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/print_buffer.h"

namespace multio {
namespace domain {

Mappings& Mappings::instance() {
    static Mappings singleton;
    return singleton;
}

void Mappings::add(message::Message msg) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    // Retrieve metadata
    auto& domainMap = mappings_[msg.name()];

    if (domainMap.contains(msg.source())) {
      eckit::Log::warning()
          << "Partial domain had already been received: " << msg.fieldId() << std::endl;
      return;
    }
    eckit::Log::debug<LibMultio>() << "*** Add domainMap for " << msg.name();

    ASSERT(not domainMap.contains(msg.source()));

    std::vector<int32_t> local_map(msg.size() / sizeof(int32_t));

    std::memcpy(local_map.data(), msg.payload().data(), msg.size());

    eckit::Log::debug<LibMultio>() << ": [";
    util::print_buffer(local_map, eckit::Log::debug<LibMultio>());
    eckit::Log::debug<LibMultio>() << "]" << std::endl;

    if (msg.metadata().getString("representation") == "unstructured") {
        domainMap.emplace(msg.source(),
                          std::make_unique<Unstructured>(std::move(local_map), msg.globalSize()));
        return;
    }

    if (msg.metadata().getString("representation") == "structured") {
        domainMap.emplace(msg.source(), std::make_unique<Structured>(std::move(local_map)));
        return;
    }

    throw eckit::AssertionFailed("Unsupported domain representation " +
                                 msg.metadata().getString("representation"));
}

void Mappings::list(std::ostream& out) const {
    auto sep = "";
    for (auto const& map : mappings_) {
        out << sep << map.first;
        sep = ", ";
    }
}

const DomainMap& Mappings::get(const std::string& name) const {
    // Must exist
    eckit::Log::debug<LibMultio>() << "*** Fetch domainMaps for " << name << std::endl;
    auto it = mappings_.find(name);
    if (it != end(mappings_)) {
        return it->second;
    }

    throw eckit::AssertionFailed("Cannot find domainMaps for " + name);
}

void Mappings::checkDomainConsistency(const std::vector<message::Message>& localDomains) const {
    if (get(localDomains.back().domain()).isConsistent()) {
        return;
    }

    std::set<int32_t> globalIndices;
    for (const auto& local : localDomains) {
        get(local.domain()).at(local.source())->collectIndices(local, globalIndices);
    }

    auto globalSize = static_cast<std::set<int32_t>::size_type>(localDomains.back().globalSize());
    if (globalIndices.size() != globalSize) {
        std::ostringstream oss;
        oss << "Number of inserted unique indices: " << globalIndices.size() << " (expected " << globalSize << ")";
        throw eckit::SeriousBug{oss.str(), Here()};
    }

    get(localDomains.back().domain()).isConsistent(true);
}

}  // namespace domain
}  // namespace multio
