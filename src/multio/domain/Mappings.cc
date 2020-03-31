
#include "Mappings.h"

#include <cstring>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"

#include "multio/library/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/print_buffer.h"

namespace multio {

Mappings& Mappings::instance() {
    static Mappings singleton;
    return singleton;
}

void Mappings::add(Message msg) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    // Retrieve metadata
    auto& mapping = mappings_[msg.name()];

    if (msg.destination().group() == "thread" && mapping.find(msg.source()) != end(mapping)) {
        // Map has been added already -- needed only for the thread transport
        return;
    }
    eckit::Log::debug<LibMultio>() << "*** Add mapping for " << msg.name();

    ASSERT(mapping.find(msg.source()) == end(mapping));

    std::vector<int32_t> local_map(msg.size() / sizeof(int32_t));

    std::memcpy(local_map.data(), msg.payload().data(), msg.size());

    eckit::Log::debug<LibMultio>() << ": [";
    print_buffer(local_map, eckit::Log::debug<LibMultio>());
    eckit::Log::debug<LibMultio>() << "]" << std::endl;

    if (msg.category() == "unstructured") {
        mapping.emplace(msg.source(), std::unique_ptr<Domain>{new Unstructured{std::move(local_map)}});
        return;
    }

    if (msg.category() == "structured") {
        mapping.emplace(msg.source(), std::unique_ptr<Domain>{new Structured{std::move(local_map)}});
        return;
    }
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
    eckit::Log::debug<LibMultio>() << "*** Fetch mappings for " << name << std::endl;
    ASSERT(mappings_.find(name) != end(mappings_));
    return mappings_.at(name);
}

}  // namespace multio
