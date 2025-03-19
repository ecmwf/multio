
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

    Average(const std::string& name, std::size_t size, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "average", size, true, win, cfg} {}

    Average(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsOptions& opt) :
        OperationWithData<T>{name, "average", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        checkTimeInterval(cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg.bitmapPresent() && cfg.options().valueCountThreshold() ? computeWithThreshold(val, cfg) : computeWithoutThreshold(val, cfg);
    }

    void updateData(const void* data, std::size_t size, const StatisticsConfiguration& cfg) override {
        checkSize(size, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const auto val = static_cast<const T*>(data);
        cfg.bitmapPresent() ? (!cfg.options().valueCountThreshold() ? updateWithMissing(val, cfg) : updateWithMissingAndCounters(val, cfg)) : updateWithoutMissing(val, cfg);
    }

private:
    void computeWithoutThreshold(T* buf, const StatisticsConfiguration& cfg) {
        std::copy(values_.begin(), values_.end(), buf);
    }

    void computeWithThreshold(T* buf, const StatisticsConfiguration& cfg) {
        const auto t = cfg.options().valueCountThreshold().value();
        const auto m = cfg.missingValue();
        const auto& counts = win_.counts();
        std::transform(values_.begin(), values_.end(), counts.begin(), buf,
                       [t, m](T v, std::int64_t c) { return static_cast<T>(c < t ? m : v); });
    }

    void updateWithoutMissing(const T* val, const StatisticsConfiguration& cfg) {
        const auto c2 = icntpp(win_.count());
        const auto c1 = sc(c2, win_.count());
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [c1, c2](T v1, T v2) { return static_cast<T>(v1 * c1 + v2 * c2); });
    }
    void updateWithMissing(const T* val, const StatisticsConfiguration& cfg) {
        const auto c2 = icntpp(win_.count());
        const auto c1 = sc(c2, win_.count());
        const auto m = cfg.missingValue();
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [c1, c2, m](T v1, T v2) { return static_cast<T>(m == v1 || m == v2 ? m : v1 * c1 + v2 * c2); });
    }
    void  updateWithMissingAndCounters(const T* val, const StatisticsConfiguration& cfg) {
        const auto m = cfg.missingValue();
        win_.updateCounts(val, values_.size(), m);
        const auto& counts = win_.counts();

        for (std::size_t i = 0; i < values_.size(); ++i) {
            if (val[i] == m) {
                continue;
            }
            const auto c = counts[i];
            const auto c2 = icntpp(c);
            const auto c1 = sc(c2, c);
            values_[i] = values_[i] * c1 + val[i] * c2;
        }
    }

    double icntpp(std::int64_t c) const { return static_cast<double>(1) / static_cast<double>(c); };
    double sc(double v, std::int64_t c) const { return static_cast<double>(c - 1) * v; };

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
