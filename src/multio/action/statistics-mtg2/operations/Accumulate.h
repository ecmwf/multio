
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics-mtg2/operations/OperationWithData.h"

namespace multio::action::statistics_mtg2 {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class Accumulate final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::checkSize;


    Accumulate(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "accumulate", sz, true, win, cfg} {}

    Accumulate(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
               const StatisticsOptions& opt) :
        OperationWithData<T>{name, "accumulate", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        buf.copy(values_.data(), values_.size() * sizeof(T));
        return;
    }

    void updateData(const void* data, long sz, const StatisticsConfiguration& cfg) override {
        checkSize(sz, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg.bitmapPresent() ? updateWithMissing(val, cfg) : updateWithoutMissing(val, cfg);
        return;
    }

private:
    void updateWithoutMissing(const T* val, const StatisticsConfiguration& cfg) {
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [](T v1, T v2) { return static_cast<T>(v1 + v2); });
        return;
    }

    void updateWithMissing(const T* val, const StatisticsConfiguration& cfg) {
        double m = cfg.missingValue();
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [m](T v1, T v2) { return static_cast<T>(m == v1 || m == v2 ? m : v1 + v2); });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action::statistics_mtg2
