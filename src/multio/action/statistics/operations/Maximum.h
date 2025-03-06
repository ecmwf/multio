
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class Maximum final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;


    Maximum(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "maximum", sz, true, win, cfg, std::numeric_limits<T>::min()} {}

    Maximum(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsOptions& opt) :
        OperationWithData<T>{name, "maximum", true, win, IOmanager, opt, std::numeric_limits<T>::min()} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        checkTimeInterval(cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg.bitmapPresent() && cfg.options().valueCountThreshold() ? computeWithThreshold(val, cfg) : computeWithoutThreshold(val, cfg);
        return;
    }

    void updateData(const void* data, long sz, const StatisticsConfiguration& cfg) override {
        checkSize(sz, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg.bitmapPresent() ? (!cfg.options().valueCountThreshold() ? updateWithMissing(val, cfg) : updateWithMissingAndCounters(val, cfg)) : updateWithoutMissing(val, cfg);
        return;
    }


private:
    void computeWithoutThreshold(T* buf, const StatisticsConfiguration& cfg) {
        std::copy(values_.begin(), values_.end(), buf);
        return;
    }

    void computeWithThreshold(T* buf, const StatisticsConfiguration& cfg) {
        const long t = cfg.options().valueCountThreshold().value();
        const double m = cfg.missingValue();
        const std::vector<long>& counts = win_.counts();
        std::transform(values_.begin(), values_.end(), counts.begin(), buf,
                       [t, m](T v, long c) { return static_cast<T>(c < t ? m : v); });
        return;
    }

    void updateWithoutMissing(const T* val, const StatisticsConfiguration& cfg) {
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [](T v1, T v2) { return static_cast<T>(v1 > v2 ? v1 : v2); });
        return;
    }

    void updateWithMissing(const T* val, const StatisticsConfiguration& cfg) {
        const double m = cfg.missingValue();
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [m](T v1, T v2) { return static_cast<T>(m == v1 || m == v2 ? m : v1 > v2 ? v1 : v2); });
        return;
    }

    void updateWithMissingAndCounters(const T* val, const StatisticsConfiguration& cfg) {
        const double m = cfg.missingValue();
        win_.updateCounts(val, values_.size(), m);
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [m](T v1, T v2) { return static_cast<T>(m == v2 ? v1 : v1 > v2 ? v1 : v2); });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; };
};

}  // namespace multio::action
