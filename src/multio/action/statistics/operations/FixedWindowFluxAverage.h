
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithDeaccumulatedData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class FixedWindowFluxAverage final : public OperationWithDeaccumulatedData<T> {
public:
    using OperationWithDeaccumulatedData<T>::name_;
    using OperationWithDeaccumulatedData<T>::cfg_;
    using OperationWithDeaccumulatedData<T>::logHeader_;
    using OperationWithDeaccumulatedData<T>::initValues_;
    using OperationWithDeaccumulatedData<T>::values_;
    using OperationWithDeaccumulatedData<T>::win_;
    using OperationWithDeaccumulatedData<T>::checkSize;
    using OperationWithDeaccumulatedData<T>::checkTimeInterval;

    FixedWindowFluxAverage(const std::string& name, long sz, const OperationWindow& win,
                           const StatisticsConfiguration& cfg) :
        OperationWithDeaccumulatedData<T>{name, "average", sz, true, win, cfg} {}

    FixedWindowFluxAverage(const std::string& name, long sz, const OperationWindow& win,
                           std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) :
        OperationWithDeaccumulatedData<T>{name, "average", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        return;
    }

    void updateData(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        std::copy(val, val + (sz / sizeof(T)), values_.begin());
        return;
    }

private:
    void computeWithMissing(T* buf) {
        const double m = cfg_.missingValue();
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep());
        std::transform(values_.begin(), values_.end(), initValues_.begin(), buf,
                       [c, m](T v1, T v2) { return static_cast<T>(m == v1 ? m : (v1 - v2) * c); });
        return;
    }

    void computeWithoutMissing(T* buf) {
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep());
        std::transform(values_.begin(), values_.end(), initValues_.begin(), buf,
                       [c](T v1, T v2) { return static_cast<T>((v1 - v2) * c); });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
