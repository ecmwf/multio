
#include "LocalIndices.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace server {

    LocalIndices::LocalIndices(std::vector<size_t>&& idx) : indices_(std::move(idx)) {}

void LocalIndices::to_global(const std::vector<double>& local, std::vector<double>& global) const {
    ASSERT(local.size() == indices_.size());

    auto it = begin(local);
    for (auto id : indices_) {
        global[id] = *it++;
    }
}

void LocalIndices::to_local(const std::vector<double>& global, std::vector<double>& local) const {
    local.resize(0);
    std::for_each(begin(indices_), end(indices_), [&](size_t id) { local.push_back(global[id]); });
}


}  // namespace server
}  // namespace multio
