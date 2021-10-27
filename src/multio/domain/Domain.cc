
#include "Domain.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"

#include "multio/message/Message.h"
#include "multio/LibMultio.h"
#include "multio/util/print_buffer.h"

namespace multio {
namespace domain {

Domain::Domain(std::vector<int32_t>&& def) : definition_(std::move(def)) {}

//------------------------------------------------------------------------------------------------------------

Unstructured::Unstructured(std::vector<int32_t>&& def) : Domain{std::move(def)} {}

void Unstructured::to_local(const std::vector<double>& global, std::vector<double>& local) const {
    local.resize(0);
    std::for_each(begin(definition_), end(definition_),
                  [&](int32_t id) { local.push_back(global[id]); });
}

void Unstructured::to_global(const message::Message& local, message::Message& global) const {
    auto levelCount = local.metadata().getLong("levelCount", 1);
    ASSERT(local.payload().size() == definition_.size() * levelCount * sizeof(double));

    auto lit = static_cast<const double*>(local.payload().data());
    auto git = static_cast<double*>(global.payload().data());
    for (long lev = 0; lev != levelCount; ++lev) {
        for (auto id : definition_) {
            auto offset = id + lev * local.globalSize();
            *(git + offset) = *lit++;
        }
    }
}

//------------------------------------------------------------------------------------------------------------

namespace {
constexpr bool inRange(int32_t val, int32_t low, int32_t upp) {
    return (low <= val) && (val < upp);
}
}  // namespace


Structured::Structured(std::vector<int32_t>&& def) : Domain{std::move(def)} {
    eckit::Log::info() << "*** Structured domain -- mapping info: ";
    util::print_buffer(definition_, eckit::Log::info());
    eckit::Log::info() << std::endl;
}

void Structured::to_local(const std::vector<double>&, std::vector<double>&) const {
    NOTIMP;
}

void Structured::to_global(const message::Message& local, message::Message& global) const {
    auto levelCount = local.metadata().getLong("levelCount", 1);

    ASSERT(definition_.size() == 11);

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
    std::ostringstream os;
    os << "Local size is " << local.payload().size() / levelCount / sizeof(double)
       << " while it is expected to equal " << data_ni << " times " << data_nj << std::endl;
    ASSERT_MSG(sizeof(double) * data_ni * data_nj * levelCount == local.size(), os.str());

    auto lit = static_cast<const double*>(local.payload().data());
    auto git = static_cast<double*>(global.payload().data());
    static std::set<int32_t> glIndices;
    static auto domainCount = 0;
    for (long lev = 0; lev != levelCount; ++lev) {
        auto offset = lev * local.globalSize();
        for (auto j = data_jbegin; j != data_jbegin + data_nj; ++j) {
            for (auto i = data_ibegin; i != data_ibegin + data_ni; ++i, ++lit) {
                if (inRange(i, 0, ni) && inRange(j, 0, nj)) {
                    auto gidx = offset + (jbegin + j) * ni_global + (ibegin + i);
                    ASSERT(glIndices.find(gidx) == std::end(glIndices));
                    glIndices.insert(gidx);
                    *(git + gidx) = *lit;
                }
            }
        }
    }
    // TODO: move this check out of here as it is rather expensive
    ++domainCount;
    if(domainCount == local.metadata().getLong("domainCount")) {
        std::ostringstream oss;
        oss << "Number of inserted unique indices: " << glIndices.size() << " (expected "
            << local.globalSize() << ")";
        eckit::Log::info() << oss.str() << std::endl;
        ASSERT_MSG(glIndices.size() == local.globalSize(), oss.str());
       glIndices.clear();
       domainCount = 0;
    }
}

//------------------------------------------------------------------------------------------------------------

Spectral::Spectral(std::vector<int32_t>&& def) : Domain{std::move(def)} {}

void Spectral::to_local(const std::vector<double>&, std::vector<double>&) const {
    NOTIMP;
}

void Spectral::to_global(const message::Message&, message::Message&) const {
    NOTIMP;
}

}  // namespace domain
}  // namespace multio
