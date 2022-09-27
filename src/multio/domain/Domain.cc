
#include "Domain.h"

#include "eckit/exception/Exceptions.h"

#include "multio/message/Message.h"
#include "multio/LibMultio.h"
#include "multio/util/print_buffer.h"

namespace multio {
namespace domain {

Domain::Domain(std::vector<int32_t>&& def) : definition_(std::move(def)) {}

//------------------------------------------------------------------------------------------------------------

Unstructured::Unstructured(std::vector<int32_t>&& def, long global_size_val) : Domain{std::move(def)}, global_size_{global_size_val} {}

void Unstructured::to_local(const std::vector<double>& global, std::vector<double>& local) const {
    local.resize(0);
    std::for_each(begin(definition_), end(definition_),
                  [&](int32_t id) { local.push_back(global[id]); });
}

void Unstructured::to_global(const message::Message& local, message::Message& global) const {
    ASSERT(local.payload().size() == definition_.size() * sizeof(double));

    auto lit = static_cast<const double*>(local.payload().data());
    auto git = static_cast<double*>(global.payload().data());
    for (auto id : definition_) {
        *(git + id) = *lit++;
    }
}

void Unstructured::to_bitmask(const message::Message&, std::vector<bool>&) const {
    NOTIMP;
}

long Unstructured::local_size() const {
   return definition_.size(); 
};
long Unstructured::global_size() const {
    return global_size_;
}

void Unstructured::collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const {
    auto payloadSize = static_cast<long>(local.payload().size() / sizeof(double));
    if (payloadSize != local_size()) {
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

void Structured::to_local(const std::vector<double>&, std::vector<double>&) const {
    NOTIMP;
}

void Structured::to_global(const message::Message& local, message::Message& global) const {
    // checkDomainConsistency(local);

    auto levelCount = local.metadata().getLong("levelCount", 1);

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

    ASSERT(sizeof(double) * ni_global * nj_global * levelCount == global.size());

    if (sizeof(double) * data_ni * data_nj * levelCount != local.size()) {
        throw eckit::AssertionFailed(
            "Local size is " +
            std::to_string(local.payload().size() / levelCount / sizeof(double)) +
            " while it is expected to equal " + std::to_string(data_ni) + " times " +
            std::to_string(data_nj));
    }

    auto lit = static_cast<const double*>(local.payload().data());
    auto git = static_cast<double*>(global.payload().data());
    for (long lev = 0; lev != levelCount; ++lev) {
        auto offset = lev * local.globalSize();
        for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
            for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++lit) {
                if (inRange(i, 0, ni) && inRange(j, 0, nj)) {
                    auto gidx = offset + (jbegin + j) * ni_global + (ibegin + i);
                    *(git + gidx) = *lit;
                }
            }
        }
    }
}

void Structured::to_bitmask(const message::Message& local, std::vector<bool>& bmask) const {
    auto levelCount = local.metadata().getLong("levelCount", 1);

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

    ASSERT(levelCount == 1);
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

    ASSERT(static_cast<std::set<int32_t>::size_type>(ni_global * nj_global) < glIndices.size());

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

long Structured::local_size() const {
    // Local domain's dimensions
    auto ni = definition_[3];
    auto nj = definition_[5];
    
    return ni*nj;
    
};
long Structured::global_size() const {
    // Global domain's dimenstions
    auto ni_global = definition_[0];
    auto nj_global = definition_[1];
    
    return ni_global*nj_global;
};


//------------------------------------------------------------------------------------------------------------

Spectral::Spectral(std::vector<int32_t>&& def) : Domain{std::move(def)} {}

void Spectral::to_local(const std::vector<double>&, std::vector<double>&) const {
    NOTIMP;
}

void Spectral::to_global(const message::Message&, message::Message&) const {
    NOTIMP;
}

void Spectral::to_bitmask(const message::Message&, std::vector<bool>&) const {
    NOTIMP;
}

long Spectral::local_size() const {
    NOTIMP;
};
long Spectral::global_size() const {
    NOTIMP;
};

void Spectral::collectIndices(const message::Message&, std::set<int32_t>&) const {
    NOTIMP;
}

}  // namespace domain
}  // namespace multio
