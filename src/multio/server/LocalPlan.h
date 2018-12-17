
#ifndef multio_server_LocalPlan_H
#define multio_server_LocalPlan_H

#include "atlas/util/Metadata.h"

#include <string>
#include <vector>

namespace multio {
namespace server {

struct LocalPlan {
    LocalPlan(const std::string& name = "atm_grid", std::vector<int> idxmap = {}) :
        mapping{idxmap} {
        metadata.set("name", std::move(name));
    }
    eckit::LocalConfiguration metadata;
    std::vector<int> mapping;
};

inline std::vector<int> create_local_to_global(size_t field_size, size_t n_proc, size_t rank) {
    // Processors on the model side -- these will be sending chunks of data to the IO servers
    auto chunk_size = field_size / n_proc + ((rank < field_size % n_proc) ? 1 : 0);

    std::vector<int> local_to_global(chunk_size);
    std::iota(begin(local_to_global), end(local_to_global), 0);
    std::for_each(begin(local_to_global), end(local_to_global),
                  [n_proc, rank](int& ii) { ii = ii * n_proc + rank; });

    return local_to_global;
}

inline LocalPlan fetch_local_plan(const atlas::util::Metadata& config, size_t n_proc, size_t rank) {
    auto local_plan = LocalPlan{};
    local_plan.metadata.set("name", config.get<std::string>("field_type"));

    auto field_type = config.get<std::string>("field_type");
    local_plan.metadata.set("name", field_type);
    if (field_type == "atm_grid") {
        local_plan.metadata.set("aggregation", "indexed");
        local_plan.metadata.set("encoding", "none");
        local_plan.metadata.set("multio_sink", "file");
        local_plan.mapping = create_local_to_global(config.get<size_t>("gl_size"), n_proc, rank);
        return local_plan;
    }

    ASSERT(false); // Nothing else is implemented as yet
    return local_plan;
}

}  // namespace server
}  // namespace multio

#endif
