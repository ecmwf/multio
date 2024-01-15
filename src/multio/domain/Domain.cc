
#include "Domain.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"

#include "multio/domain/MaskCompression.h"
#include "multio/message/Message.h"
#include "multio/util/VariantHelpers.h"

namespace multio {
namespace domain {

Domain::Domain(std::vector<int32_t>&& def) : definition_(std::move(def)) {}

//------------------------------------------------------------------------------------------------------------

Unstructured::Unstructured(std::vector<int32_t>&& def, std::int64_t globalSize_val) :
    Domain{std::move(def)}, globalSize_{globalSize_val} {}

void Unstructured::toLocal(const std::vector<double>& global, std::vector<double>& local) const {
    local.resize(0);
    std::for_each(begin(definition_), end(definition_), [&](int32_t id) { local.push_back(global[id]); });
}

void Unstructured::toGlobal(const message::Message& local, message::Message& global) const {
    dispatchPrecisionTag(local.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        toGlobalImpl<Precision>(local, global);
    });
}

void Unstructured::toBitmask(const message::Message&, std::vector<bool>&) const {
    NOTIMP;
}

std::int64_t Unstructured::localSize() const {
    return definition_.size();
}

std::int64_t Unstructured::globalSize() const {
    return globalSize_;
}

std::int64_t Unstructured::partialSize() const {
    return globalSize_;
}

void Unstructured::collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const {
    const auto dataSize = (local.precision() == util::PrecisionTag::Float) ? sizeof(float) : sizeof(double);
    const auto payloadSize = static_cast<std::int64_t>(local.payload().size() / dataSize);
    if (payloadSize != localSize()) {
        throw eckit::SeriousBug{"Mismatch between sizes of index map and local field", Here()};
    }

    for (const auto& idx : definition_) {
        glIndices.insert(idx);
    }
}

template <typename Precision>
void Unstructured::toGlobalImpl(const message::Message& local, message::Message& global) const {
    ASSERT(local.payload().size() == definition_.size() * sizeof(Precision));

    auto lit = static_cast<const Precision*>(local.payload().data());
    auto git = static_cast<Precision*>(global.payload().modifyData());
    for (auto id : definition_) {
        *(git + id) = *lit++;
    }
}


//------------------------------------------------------------------------------------------------------------

namespace {
constexpr bool inRange(int32_t val, int32_t low, int32_t upp) {
    return (low <= val) && (val < upp);
}

inline std::vector<int32_t>&& addPartialDomainSizeToDefinition(std::vector<int32_t>&& def) {
    if (def.size() == 11) {
        // The 12entry is ment to be to partial size of the grid. If it is not set, it is assumed to be equal to the
        // the global size which is computed by ni_global*nj_global
        def.push_back(def[0] * def[1]);
    }
    return std::move(def);
}
}  // namespace


Structured::Structured(std::vector<int32_t>&& def) : Domain{addPartialDomainSizeToDefinition(std::move(def))} {
    ASSERT((definition_.size() == 12));
}

void Structured::toLocal(const std::vector<double>&, std::vector<double>&) const {
    NOTIMP;
}

void Structured::toGlobal(const message::Message& local, message::Message& global) const {
    dispatchPrecisionTag(local.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        toGlobalImpl<Precision>(local, global);
    });
}

void Structured::toBitmask(const message::Message& local, std::vector<bool>& bmask) const {

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

    auto data_partial_size = definition_[11];

    ASSERT(data_partial_size <= (ni_global * nj_global));

    ASSERT(static_cast<std::set<int32_t>::size_type>(ni_global * nj_global) == bmask.size());

    EncodedMaskPayload encodedMaskPayload(local.payload());
    std::size_t expectedBitmaskSize = data_nj * data_ni;
    if (encodedMaskPayload.size() != (data_nj * data_ni)) {
        std::ostringstream oss;
        oss << "Structured::toBitmask: The bitmask has a size of " << encodedMaskPayload.size()
            << " but is expected to have a size of " << expectedBitmaskSize << std::endl;
        throw eckit::SeriousBug{oss.str(), Here()};
    }

    auto lit = encodedMaskPayload.begin();
    for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
        for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++lit) {
            if (inRange(i, 0, ni) && inRange(j, 0, nj)) {
                auto bidx = (jbegin + j) * ni_global + (ibegin + i);
                bmask[bidx] = *lit;
            }
        }
    }
}

void Structured::collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const {
    dispatchPrecisionTag(local.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        collectIndicesImpl<Precision>(local, glIndices);
    });
}

template <typename Precision>
void Structured::toGlobalImpl(const message::Message& local, message::Message& global) const {
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

    ASSERT(sizeof(Precision) * ni_global * nj_global == global.size());

    if (sizeof(Precision) * data_ni * data_nj != local.size()) {
        throw eckit::AssertionFailed("Local size is " + std::to_string(local.payload().size() / sizeof(Precision))
                                     + " while it is expected to equal " + std::to_string(data_ni) + " times "
                                     + std::to_string(data_nj));
    }

    auto lit = static_cast<const Precision*>(local.payload().data());
    auto git = static_cast<Precision*>(global.payload().modifyData());
    for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
        for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++lit) {
            if (inRange(i, 0, ni) && inRange(j, 0, nj)) {
                auto gidx = (jbegin + j) * ni_global + (ibegin + i);
                *(git + gidx) = *lit;
            }
        }
    }
}

template <typename Precision>
void Structured::collectIndicesImpl(const message::Message& local, std::set<int32_t>& glIndices) const {
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

    ASSERT(glIndices.size() < static_cast<std::set<int32_t>::size_type>(ni_global * nj_global));

    auto payloadSize = static_cast<std::int64_t>(local.payload().size() / sizeof(Precision));
    if (payloadSize != data_ni * data_nj) {  // Payload contains halo information
        throw eckit::SeriousBug{"Mismatch between sizes of index map and local field", Here()};
    }

    auto lit = static_cast<const Precision*>(local.payload().data());
    for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
        for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++lit) {
            if (inRange(i, 0, ni) && inRange(j, 0, nj)) {
                auto gidx = (jbegin + j) * ni_global + (ibegin + i);
                ASSERT(glIndices.find(gidx) == std::end(glIndices));
                glIndices.insert(gidx);
            }
        }
    }
}

std::int64_t Structured::localSize() const {
    // Local domain's dimensions
    auto ni = definition_[3];
    auto nj = definition_[5];

    return ni * nj;
};

std::int64_t Structured::globalSize() const {
    // Global domain's dimenstions
    auto ni_global = definition_[0];
    auto nj_global = definition_[1];

    return ni_global * nj_global;
};

std::int64_t Structured::partialSize() const {
    return definition_[11];
}

//------------------------------------------------------------------------------------------------------------

Spectral::Spectral(std::vector<int32_t>&& def) : Domain{std::move(def)} {}

void Spectral::toLocal(const std::vector<double>&, std::vector<double>&) const {
    NOTIMP;
}

void Spectral::toGlobal(const message::Message&, message::Message&) const {
    NOTIMP;
}

void Spectral::toBitmask(const message::Message&, std::vector<bool>&) const {
    NOTIMP;
}

std::int64_t Spectral::localSize() const {
    NOTIMP;
};
std::int64_t Spectral::globalSize() const {
    NOTIMP;
};
std::int64_t Spectral::partialSize() const {
    NOTIMP;
};

void Spectral::collectIndices(const message::Message&, std::set<int32_t>&) const {
    NOTIMP;
}

}  // namespace domain
}  // namespace multio
