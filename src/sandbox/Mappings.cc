
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

void Mappings::add(Message msg) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    eckit::Log::info() << "  ---  name = " << msg.mapping() << std::endl;
    eckit::Log::info() << "  ---  peer = " << msg.source() << std::endl;

    // Retrieve metadata
    auto& mapping = mappings_[msg.mapping()];
    ASSERT(mapping.find(msg.source()) == end(mapping));

    auto& local_map = mapping[msg.source()];
    local_map.resize(msg.size() / sizeof(int));

    std::memcpy(local_map.data(), msg.payload().data(), msg.size());

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

}  // namespace sandbox
}  // namespace multio
