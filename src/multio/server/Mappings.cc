
#include "Mappings.h"

#include <cstring>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"

#include "multio/server/LocalIndices.h"
#include "multio/server/Message.h"

namespace multio {
namespace server {

Mappings& Mappings::instance() {
    static Mappings singleton;
    return singleton;
}

void Mappings::add(Message msg) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    eckit::Log::info() << "  ---  name = " << msg.mapping() << std::endl;
    eckit::Log::info() << "  ---  peer = " << msg.source() << std::endl;

    // Retrieve metadata
    auto& mapping = mappings_[msg.mapping()];

    if (msg.destination().domain_ == "thread" && mapping.find(msg.source()) != end(mapping)) {
        // Map has been added already -- needed only for the thread transport
        return;
    }
    ASSERT(mapping.find(msg.source()) == end(mapping));

    std::vector<size_t> local_map(msg.size() / sizeof(size_t));

    std::memcpy(local_map.data(), msg.payload().data(), msg.size());

    mapping.emplace(msg.source(), std::move(local_map));

    eckit::Log::info() << "  ---  local-map size = " << local_map.size() << std::endl;

    eckit::Log::info() << "  ---  map size = " << mappings_.at("scattered").size() << std::endl;
}

void Mappings::list(std::ostream& out) const {
    auto sep = "";
    for (auto const& map : mappings_) {
        out << sep << map.first;
        sep = ", ";
    }
}

auto Mappings::get(const std::string& name) const -> const Mapping& {
    // Must exist
    ASSERT(mappings_.find(name) != end(mappings_));
    return mappings_.at(name);
}

}  // namespace server
}  // namespace multio
