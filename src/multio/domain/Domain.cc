
#include "Domain.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"

#include "multio/library/LibMultio.h"

namespace multio {

Domain::Domain(std::vector<int32_t>&& def) : definition_(std::move(def)) {}

//------------------------------------------------------------------------------------------------------------

Unstructured::Unstructured(std::vector<int32_t>&& def) : Domain{std::move(def)} {}

void Unstructured::to_local(const std::vector<double>& global, std::vector<double>& local) const {
    local.resize(0);
    std::for_each(begin(definition_), end(definition_),
                  [&](int32_t id) { local.push_back(global[id]); });
}

void Unstructured::to_global(const eckit::Buffer& local, eckit::Buffer& global) const {
    ASSERT(local.size() == definition_.size() * sizeof(double));

    auto lit = static_cast<const double*>(local.data());
    auto git = static_cast<double*>(global.data());
    for (auto id : definition_) {
        *(git + id) = *lit++;
    }
    eckit::Log::debug<LibMultio>() << " *** Aggregation completed..." << std::endl;
}

//------------------------------------------------------------------------------------------------------------

namespace {
constexpr bool inRange(int32_t val, int32_t low, int32_t upp) {
    return (low <= val) && (val < upp);
}
}  // namespace


Structured::Structured(std::vector<int32_t>&& def) : Domain{std::move(def)} {}

void Structured::to_local(const std::vector<double>& global, std::vector<double>& local) const {
    NOTIMP;
}

void Structured::to_global(const eckit::Buffer& local, eckit::Buffer& global) const {
    // Global domain's dimenstions
    auto ni_global = definition_[0];
    auto nj_global = definition_[1];

    // Local domain's dimensions
    auto ibegin = definition_[2];
    auto ni = definition_[3];
    auto jbegin = definition_[4];
    auto nj = definition_[5];

    // Data dimensions on local domain -- includes halo points
    auto data_ibegin = definition_[7];
    auto data_ni = definition_[8];
    auto data_jbegin = definition_[9];
    auto data_nj = definition_[10];
    // auto data_dim = definition_[6]; -- Unused here

    ASSERT(sizeof(double) * ni_global * nj_global == global.size());
    ASSERT(sizeof(double) * data_ni * data_nj == local.size());

    auto lit = static_cast<const double*>(local.data());
    auto git = static_cast<double*>(global.data());
    for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
        for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++lit) {
            if (inRange(i, 0, ni) && inRange(j, 0, nj)) {
                auto gidx = (jbegin + j) * ni_global + (ibegin + i);
                *(git + gidx) = *lit;
            }
        }
    }
}

//------------------------------------------------------------------------------------------------------------

Spectral::Spectral(std::vector<int32_t>&& def) : Domain{std::move(def)} {}

void Spectral::to_local(const std::vector<double>& global, std::vector<double>& local) const {
    NOTIMP;
}

void Spectral::to_global(const eckit::Buffer&, eckit::Buffer&) const {
    NOTIMP;
}

}  // namespace multio
