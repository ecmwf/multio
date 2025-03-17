
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithDeaccumulatedData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class DeAccumulate final : public OperationWithDeaccumulatedData<T> {
public:
    using OperationWithDeaccumulatedData<T>::name_;
    using OperationWithDeaccumulatedData<T>::logHeader_;
    using OperationWithDeaccumulatedData<T>::initValues_;
    using OperationWithDeaccumulatedData<T>::values_;
    using OperationWithDeaccumulatedData<T>::win_;
    using OperationWithDeaccumulatedData<T>::checkSize;
    using OperationWithDeaccumulatedData<T>::checkTimeInterval;


    DeAccumulate(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithDeaccumulatedData<T>{name, "accumulate", sz, true, win, cfg} {}

    DeAccumulate(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                 const StatisticsOptions& opt) :
        OperationWithDeaccumulatedData<T>{name, "accumulate", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        checkTimeInterval(cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg.bitmapPresent() ? computeWithMissing(val, cfg) : computeWithoutMissing(val, cfg);
    }

    void updateData(const void* data, long sz, const StatisticsConfiguration& cfg) override {
        checkSize(sz, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        std::copy(val, val + (sz / sizeof(T)), values_.begin());
    }

private:
    void computeWithoutMissing(T* val, const StatisticsConfiguration& cfg) {
        std::transform(values_.begin(), values_.end(), initValues_.begin(), val,
                       [](T v1, T v2) { return static_cast<T>(v1 - v2); });
    }

    void computeWithMissing(T* val, const StatisticsConfiguration& cfg) {
        double m = cfg.missingValue();
        std::transform(values_.begin(), values_.end(), initValues_.begin(), val,
                       [m](T v1, T v2) { return static_cast<T>(m == v1 || m == v2 ? m : v1 - v2); });
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
