
#ifndef multio_attic_PartialMapping_H
#define multio_attic_PartialMapping_H

#include "eckit/exception/Exceptions.h"

#include "atlas/util/Metadata.h"

#include <string>
#include <vector>
#include <numeric>
#include <algorithm>

namespace multio {
namespace attic {

struct PartialMapping {
    PartialMapping(const std::string& name = "scattered", std::vector<int> idxmap = {}) :
        indices{idxmap} {
        metadata.set("name", name);
    }
    std::string name() {
        std::string name;
        metadata.get("name", name);
        return name;
    }
    size_t map_count() {
        size_t count;
        metadata.get("map_count", count);
        return count;
    }
    eckit::LocalConfiguration metadata;
    std::vector<int> indices;
};

inline std::vector<int> create_partial_mapping(size_t field_size, size_t n_proc, size_t rank) {
    // Processors on the model side -- these will be sending chunks of data to the IO servers
    auto chunk_size = field_size / n_proc + ((rank < field_size % n_proc) ? 1 : 0);

    std::vector<int> local_to_global(chunk_size);
    std::iota(begin(local_to_global), end(local_to_global), 0);
    std::for_each(begin(local_to_global), end(local_to_global), [n_proc, rank](int& ii) {
        ii = ii * static_cast<int>(n_proc) + static_cast<int>(rank);
    });

    return local_to_global;
}

inline PartialMapping fetch_mapping(const atlas::util::Metadata& config, size_t n_proc,
                                    size_t rank) {
    auto partial_mapping =
        PartialMapping{config.get<std::string>("mapping"),
                       create_partial_mapping(config.get<size_t>("global_size"), n_proc, rank)};

    partial_mapping.metadata.set("map_count", n_proc);

    return partial_mapping;
}

}  // namespace attic
}  // namespace multio

#endif
