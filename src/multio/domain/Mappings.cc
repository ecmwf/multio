
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

    if (domainMap.find(msg.source()) != end(domainMap)) {
        // Map has been added already
        return;
    }
    eckit::Log::debug<LibMultio>() << "*** Add domainMap for " << msg.name();

    ASSERT(domainMap.find(msg.source()) == end(domainMap));

    std::vector<int32_t> local_map(msg.size() / sizeof(int32_t));

    std::memcpy(local_map.data(), msg.payload().data(), msg.size());

    eckit::Log::debug<LibMultio>() << ": [";
    util::print_buffer(local_map, eckit::Log::debug<LibMultio>());
    eckit::Log::debug<LibMultio>() << "]" << std::endl;

    if (msg.category() == "unstructured") {
        domainMap.emplace(msg.source(),
                        std::unique_ptr<Domain>{new Unstructured{std::move(local_map)}});
        return;
    }

    if (msg.category() == "structured") {
        domainMap.emplace(msg.source(),
                        std::unique_ptr<Domain>{new Structured{std::move(local_map)}});
        return;
    }

    throw eckit::AssertionFailed("Unsupported domain category" + msg.category());
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

}  // namespace domain
}  // namespace multio
