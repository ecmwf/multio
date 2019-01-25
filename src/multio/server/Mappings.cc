
#include "Mappings.h"

#include "atlas/util/Metadata.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"

#include "multio/server/Message.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Mappings& Mappings::instance() {
    static Mappings singleton;
    return singleton;
}

void Mappings::add(const Message& msg) {
    std::lock_guard<std::mutex> lock{mutex_};

    ASSERT(msg.tag() == msg_tag::message_data);

    // Retrieve metadata
    auto meta_size = 0ul;
    msg.read(&meta_size, sizeof(unsigned long));
    auto meta_buf = eckit::Buffer(meta_size);
    msg.read(meta_buf, meta_size);
    auto metadata = atlas::util::Metadata{unpack_metadata(meta_buf)};

    auto name = metadata.get<std::string>("name");
    if (mappings_.find(name) == end(mappings_)) {
        auto map_count = metadata.get<size_t>("map_count");
        mappings_[name] = std::vector<LocalIndices>{map_count};
    }

    auto data_size = msg.size() - meta_size - sizeof(unsigned long);
    auto& local_map = mappings_.at(name)[msg.peer()];

    ASSERT(local_map.empty());

    local_map.resize(data_size / sizeof(int));
    ASSERT(!local_map.empty());
    msg.read(local_map.data(), data_size);
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
