
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class Average final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;

    Average(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "average", sz, true, win, cfg} {}

    Average(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsOptions& opt) :
        OperationWithData<T>{name, "average", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        checkTimeInterval(cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg.bitmapPresent() && cfg.options().valueCountThreshold() > 0 ? computeWithThreshold(val, cfg) : computeWithoutThreshold(val, cfg);
        return;
    }

    void updateData(const void* data, long sz, const StatisticsConfiguration& cfg) override {
        checkSize(sz, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg.bitmapPresent() ? (cfg.options().valueCountThreshold() < 0 ? updateWithMissing(val, cfg) : updateWithMissingAndCounters(val, cfg)) : updateWithoutMissing(val, cfg);
        return;
    }

private:
    void computeWithoutThreshold(T* buf, const StatisticsConfiguration& cfg) {
        std::copy(values_.begin(), values_.end(), buf);
        return;
    }

    void computeWithThreshold(T* buf, const StatisticsConfiguration& cfg) {
        const long t = cfg.options().valueCountThreshold();
        const double m = cfg.missingValue();
        const std::vector<long>& counts = win_.counts();
        std::transform(values_.begin(), values_.end(), counts.begin(), buf,
                       [t, m](T v, long c) { return static_cast<T>(c < t ? m : v); });
        return;
    }

    void updateWithoutMissing(const T* val, const StatisticsConfiguration& cfg) {
        const double c2 = icntpp(win_.count()), c1 = sc(c2, win_.count());
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [c1, c2](T v1, T v2) { return static_cast<T>(v1 * c1 + v2 * c2); });
        return;
    }
    void updateWithMissing(const T* val, const StatisticsConfiguration& cfg) {
        const double c2 = icntpp(win_.count()), c1 = sc(c2, win_.count()), m = cfg.missingValue();
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [c1, c2, m](T v1, T v2) { return static_cast<T>(m == v1 || m == v2 ? m : v1 * c1 + v2 * c2); });
        return;
    }
    void  updateWithMissingAndCounters(const T* val, const StatisticsConfiguration& cfg) {
        const double m = cfg.missingValue();
        win_.updateCounts(val, values_.size(), m);
        const std::vector<long>& counts = win_.counts();

        for (size_t i = 0; i < values_.size(); ++i) {
            if (val[i] == m) { continue; }
            const double c = counts[i], c2 = icntpp(c), c1 = sc(c2, c);
            values_[i] = values_[i] * c1 + val[i] * c2;
        }
        return;
    }

    double icntpp(long c) const { return double(1.0) / double(c); };
    double sc(double v, long c) const { return double(c - 1) * v; };
    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
