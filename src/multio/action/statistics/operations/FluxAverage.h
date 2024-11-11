
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class FluxAverage final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;

    FluxAverage(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "average", sz, true, win, cfg} {}

    FluxAverage(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                const StatisticsOptions& opt) :
        OperationWithData<T>{name, "average", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        checkTimeInterval(cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg.bitmapPresent() ? computeWithMissing(val,cfg) : computeWithoutMissing(val,cfg);
        return;
    }

    void updateData(const void* data, long sz, const StatisticsConfiguration& cfg) override {
        checkSize(sz,cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        std::copy(val, val + (sz / sizeof(T)), values_.begin());
        return;
    }

private:
    void computeWithMissing(T* buf, const StatisticsConfiguration& cfg) {
        const double m = cfg.missingValue();
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg.stepFreq() * cfg.timeStep());
        std::transform(values_.begin(), values_.end(), buf, [c, m](T v) { return static_cast<T>(m == v ? m : v * c); });
        return;
    }

    void computeWithoutMissing(T* buf, const StatisticsConfiguration& cfg) {
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg.stepFreq() * cfg.timeStep());
        std::transform(values_.begin(), values_.end(), buf, [c](T v) { return static_cast<T>(v * c); });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
