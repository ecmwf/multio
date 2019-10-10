
#include "LocalIndices.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"

namespace multio {
namespace server {

LocalIndices::LocalIndices(std::vector<int32_t>&& idx) : indices_(std::move(idx)) {}

//------------------------------------------------------------------------------------------------------------

Unstructured::Unstructured(std::vector<int32_t>&& idx) : LocalIndices{std::move(idx)} {}

void Unstructured::to_global(const std::vector<double>& local, std::vector<double>& global) const {
    eckit::Log::debug<LibMultio>() << " *** Aggregator fields size:  " << local.size() << std::endl;
    eckit::Log::debug<LibMultio>()
        << " *** Aggregator indices size: " << indices_.size() << std::endl;

    ASSERT(local.size() == indices_.size());

    auto it = begin(local);
    for (auto id : indices_) {
        global[id] = *it++;
    }
}

void Unstructured::to_local(const std::vector<double>& global, std::vector<double>& local) const {
    local.resize(0);
    std::for_each(begin(indices_), end(indices_), [&](int32_t id) { local.push_back(global[id]); });
}

//------------------------------------------------------------------------------------------------------------

Structured::Structured(std::vector<int32_t>&& idx) : LocalIndices{std::move(idx)} {}

void Structured::to_global(const std::vector<double>& local, std::vector<double>& global) const {
    eckit::Log::debug<LibMultio>() << " *** Aggregator fields size:  " << local.size() << std::endl;
    eckit::Log::debug<LibMultio>()
        << " *** Aggregator indices size: " << indices_.size() << std::endl;

    const auto& ni_global = indices_[0];
    const auto& nj_global = indices_[1];
    const auto& ibegin = indices_[2];
    const auto& ni = indices_[3];
    const auto& jbegin = indices_[4];
    const auto& nj = indices_[5];

    // const auto& data_dim = indices_[6]; -- Unused here
    const auto& data_ibegin = indices_[7];
    const auto& data_ni = indices_[8];
    const auto& data_jbegin = indices_[9];
    const auto& data_nj = indices_[10];

    ASSERT(ni_global * nj_global == global.size());
    ASSERT(data_ni * data_nj == local.size());

    auto it = begin(local);
    auto counter = 0u;
    eckit::Log::debug<LibMultio>() << "              ni = " << ni << std::endl;
    eckit::Log::debug<LibMultio>() << "              nj = " << nj << std::endl;
    eckit::Log::debug<LibMultio>() << "     data_ibegin = " << data_ibegin << std::endl;
    eckit::Log::debug<LibMultio>() << "         data_ni = " << data_ni << std::endl;
    eckit::Log::debug<LibMultio>() << "     data_jbegin = " << data_jbegin << std::endl;
    eckit::Log::debug<LibMultio>() << "         data_nj = " << data_nj << std::endl;
    for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
        for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++it) {
            if ((i < 0) || (j < 0) || (ni <= i) || (nj <= j)) {
                continue;  // Ghost data -- ignore
            }
            ++counter;
            auto gidx = (jbegin + j) * ni_global + (ibegin + i);
            global[gidx] = *it;
        }
    }
    ASSERT(counter == ni * nj);
    eckit::Log::debug<LibMultio>()
        << "Number of values having been put into global field: " << counter << std::endl;
}

void Structured::to_local(const std::vector<double>& global, std::vector<double>& local) const {
    NOTIMP;
}

}  // namespace server
}  // namespace multio
