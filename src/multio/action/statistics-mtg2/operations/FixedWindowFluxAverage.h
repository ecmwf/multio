
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics-mtg2/operations/OperationWithDeaccumulatedData.h"

namespace multio::action::statistics_mtg2 {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class FixedWindowFluxAverage final : public OperationWithDeaccumulatedData<T> {
public:
    using OperationWithDeaccumulatedData<T>::name_;
    using OperationWithDeaccumulatedData<T>::logHeader_;
    using OperationWithDeaccumulatedData<T>::initValues_;
    using OperationWithDeaccumulatedData<T>::values_;
    using OperationWithDeaccumulatedData<T>::win_;
    using OperationWithDeaccumulatedData<T>::checkSize;

    FixedWindowFluxAverage(const std::string& name, long sz, const OperationWindow& win,
                           const StatisticsConfiguration& cfg) :
        OperationWithDeaccumulatedData<T>{name, "average", sz, true, win, cfg} {}

    FixedWindowFluxAverage(const std::string& name, const OperationWindow& win,
                           std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) :
        OperationWithDeaccumulatedData<T>{name, "average", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg.bitmapPresent() ? computeWithMissing(val, cfg) : computeWithoutMissing(val, cfg);
        return;
    }

    void updateData(const void* data, long sz, const StatisticsConfiguration& cfg) override {
        checkSize(sz, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        std::copy(val, val + (sz / sizeof(T)), values_.begin());
        return;
    }

private:
    void computeWithMissing(T* buf, const StatisticsConfiguration& cfg) {
        const double m = cfg.missingValue();
        const double c = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg.stepFreq() * cfg.timeStep());
        std::transform(values_.begin(), values_.end(), initValues_.begin(), buf,
                       [c, m](T v1, T v2) { return static_cast<T>(m == v1 || m == v2 ? m : (v1 - v2) * c); });
        return;
    }

    void computeWithoutMissing(T* buf, const StatisticsConfiguration& cfg) {
        const double c = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg.stepFreq() * cfg.timeStep());
        std::transform(values_.begin(), values_.end(), initValues_.begin(), buf,
                       [c](T v1, T v2) { return static_cast<T>((v1 - v2) * c); });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action::statistics_mtg2
