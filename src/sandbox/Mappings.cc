
#include "Mappings.h"

#include <cstring>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"

#include "sandbox/Message.h"

namespace multio {
namespace sandbox {

Mappings& Mappings::instance() {
    static Mappings singleton;
    return singleton;
}

void Mappings::add(const Message& msg) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    // Retrieve metadata
    auto name = msg.mapping();
    if (mappings_.find(name) == end(mappings_)) {
        auto map_count = static_cast<size_t>(msg.map_count());
        mappings_[name] = std::vector<LocalIndices>{map_count};
    }

    eckit::Log::info() << "  ---  name = " << name << std::endl;
    auto& local_map = mappings_.at(name)[msg.source().id_];
    eckit::Log::info() << "  ---  number of maps: " << mappings_.at(name).size() << std::endl;

    eckit::Log::info() << "  ---  id_ = " << msg.source().id_ << std::endl;

    ASSERT(local_map.empty());

    local_map.resize(msg.size() / sizeof(int));
    ASSERT(!local_map.empty());

    std::memcpy(local_map.data(), msg.payload(), msg.size());
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

}  // namespace sandbox
}  // namespace multio
