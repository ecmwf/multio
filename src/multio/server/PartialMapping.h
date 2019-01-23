
#ifndef multio_server_PartialMapping_H
#define multio_server_PartialMapping_H

#include "eckit/exception/Exceptions.h"

#include "atlas/util/Metadata.h"

#include <string>
#include <vector>
#include <numeric>
#include <algorithm>

namespace multio {
namespace server {

struct PartialMapping {
    PartialMapping(const std::string& name = "atm_grid", std::vector<int> idxmap = {}) :
        indices{idxmap} {
        metadata.set("plan_name", name);
    }
    std::string plan_name() {
        std::string name;
        metadata.get("plan_name", name);
        return name;
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
        PartialMapping{config.get<std::string>("plan_name"),
                       create_partial_mapping(config.get<size_t>("gl_size"), n_proc, rank)};

    partial_mapping.metadata.set("no_maps", n_proc);

    return partial_mapping;
}

}  // namespace server
}  // namespace multio

#endif
