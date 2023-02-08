
#include "Domain.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"

#include "multio/message/Message.h"

namespace multio {
namespace domain {

Domain::Domain(std::vector<int32_t>&& def) : definition_(std::move(def)) {}

//------------------------------------------------------------------------------------------------------------

Unstructured::Unstructured(std::vector<int32_t>&& def, long globalSize_val) :
    Domain{std::move(def)}, globalSize_{globalSize_val} {}

void Unstructured::toLocal(const std::vector<double>& global, std::vector<double>& local) const {
    local.resize(0);
    std::for_each(begin(definition_), end(definition_),
                  [&](int32_t id) { local.push_back(global[id]); });
}

void Unstructured::toGlobal(const message::Message& local, message::Message& global) const {
    ASSERT(local.payload().size() == definition_.size() * sizeof(double));

    auto lit = static_cast<const double*>(local.payload().data());
    auto git = static_cast<double*>(global.payload().data());
    for (auto id : definition_) {
        *(git + id) = *lit++;
    }
}

void Unstructured::toBitmask(const message::Message&, std::vector<bool>&) const {
    NOTIMP;
}

long Unstructured::localSize() const {
   return definition_.size();
};

long Unstructured::globalSize() const {
    return globalSize_;
}

void Unstructured::collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const {
    auto payloadSize = static_cast<long>(local.payload().size() / sizeof(double));
    if (payloadSize != localSize()) {
        throw eckit::SeriousBug{"Mismatch between sizes of index map and local field", Here()};
    }

    for(const auto& idx : definition_) {
        glIndices.insert(idx);
    }
}


//------------------------------------------------------------------------------------------------------------

namespace {
constexpr bool inRange(int32_t val, int32_t low, int32_t upp) {
    return (low <= val) && (val < upp);
}
}  // namespace


Structured::Structured(std::vector<int32_t>&& def) : Domain{std::move(def)} {
    ASSERT(definition_.size() == 11);
}

void Structured::toLocal(const std::vector<double>&, std::vector<double>&) const {
    NOTIMP;
}

void Structured::toGlobal(const message::Message& local, message::Message& global) const {

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

    if (sizeof(double) * data_ni * data_nj != local.size()) {
        throw eckit::AssertionFailed("Local size is " + std::to_string(local.payload().size() / sizeof(double))
                                     + " while it is expected to equal " + std::to_string(data_ni) + " times "
                                     + std::to_string(data_nj));
    }

    auto lit = static_cast<const double*>(local.payload().data());
    auto git = static_cast<double*>(global.payload().data());
    for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
        for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++lit) {
            if (inRange(i, 0, ni) && inRange(j, 0, nj)) {
                auto gidx = (jbegin + j) * ni_global + (ibegin + i);
                *(git + gidx) = *lit;
            }
        }
    }
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

    ASSERT(static_cast<std::set<int32_t>::size_type>(ni_global * nj_global) == bmask.size());
    auto lit = static_cast<const uint8_t*>(local.payload().data());
    for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
        for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++lit) {
            if (inRange(i, 0, ni) && inRange(j, 0, nj)) {
                auto bidx = (jbegin + j) * ni_global + (ibegin + i);
                bmask[bidx] = (*lit == 0.0) ? false : true;
            }
        }
    }
}


void Structured::collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const {

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

    auto payloadSize = static_cast<long>(local.payload().size() / sizeof(double));
    if (payloadSize != data_ni * data_nj) { // Payload contains halo informat$ion
        throw eckit::SeriousBug{"Mismatch between sizes of index map and local field", Here()};
    }

    auto lit = static_cast<const double*>(local.payload().data());
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

long Structured::localSize() const {
    // Local domain's dimensions
    auto ni = definition_[3];
    auto nj = definition_[5];

    return ni*nj;

};
long Structured::globalSize() const {
    // Global domain's dimenstions
    auto ni_global = definition_[0];
    auto nj_global = definition_[1];

    return ni_global*nj_global;
};


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

long Spectral::localSize() const {
    NOTIMP;
};
long Spectral::globalSize() const {
    NOTIMP;
};

void Spectral::collectIndices(const message::Message&, std::set<int32_t>&) const {
    NOTIMP;
}

}  // namespace domain
}  // namespace multio
