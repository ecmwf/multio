
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

    FluxAverage(const std::string& name, std::size_t size, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "average", size, true, win, cfg} {}

    FluxAverage(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                const StatisticsOptions& opt) :
        OperationWithData<T>{name, "average", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        checkTimeInterval(cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg.bitmapPresent() ? computeWithMissing(val, cfg) : computeWithoutMissing(val, cfg);
    }

    void updateData(const void* data, std::size_t size, const StatisticsConfiguration& cfg) override {
        checkSize(size, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const auto val = static_cast<const T*>(data);
        std::copy(val, val + (size / sizeof(T)), values_.begin());
    }

private:
    void computeWithMissing(T* buf, const StatisticsConfiguration& cfg) {
        const auto m = cfg.missingValue();
        const auto c = static_cast<double>(1) / static_cast<double>(win_.count() * cfg.stepFreq() * cfg.timeStep());
        std::transform(values_.begin(), values_.end(), buf, [c, m](T v) { return static_cast<T>(m == v ? m : v * c); });
    }

    void computeWithoutMissing(T* buf, const StatisticsConfiguration& cfg) {
        const auto c = static_cast<double>(1) / static_cast<double>(win_.count() * cfg.stepFreq() * cfg.timeStep());
        std::transform(values_.begin(), values_.end(), buf, [c](T v) { return static_cast<T>(v * c); });
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
