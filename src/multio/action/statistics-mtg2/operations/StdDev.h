
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics-mtg2/operations/OperationWithData.h"

namespace multio::action::statistics_mtg2 {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class StdDev final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::checkSize;

    StdDev(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "stddev", sz, true, win, cfg}, mean_{std::vector<T>(sz /= sizeof(T), 0.0)} {}

    StdDev(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsOptions& opt) :
        OperationWithData<T>{name, "stddev", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg.bitmapPresent() && cfg.options().valueCountThreshold() ? computeWithThreshold(val, cfg) : computeWithoutThreshold(val, cfg);
    }

    void updateData(const void* data, long sz, const StatisticsConfiguration& cfg) override {
        checkSize(sz, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg.bitmapPresent() ? (!cfg.options().valueCountThreshold() ? updateWithMissing(val, cfg) : updateWithMissingAndCounters(val, cfg)) : updateWithoutMissing(val, cfg);
    }

private:
    void computeWithoutThreshold(T* buf, const StatisticsConfiguration& cfg) {
        const auto c = 1.0 / win_.count();
        std::transform(values_.begin(), values_.end(), buf,
                       [c](T v) { return std::sqrt(v * c); });
    }

    void computeWithThreshold(T* buf, const StatisticsConfiguration& cfg) {
        const auto t = cfg.options().valueCountThreshold().value();
        const auto m = cfg.missingValue();
        const auto& counts = win_.counts();
        std::transform(values_.begin(), values_.end(), counts.begin(), buf,
                       [t, m](T v, auto c) { return static_cast<T>(c < t ? m : std::sqrt(v / c)); });
    }

    void updateWithoutMissing(const T* val, const StatisticsConfiguration& cfg) {
        const auto c = 1.0 / win_.count();
        for (size_t i = 0; i < values_.size(); ++i) {
            const auto oldMean = mean_[i];
            mean_[i] += c * (val[i] - oldMean);
            values_[i] += (val[i] - oldMean) * (val[i] - mean_[i]);
        }
    }
    void updateWithMissing(const T* val, const StatisticsConfiguration& cfg) {
        const auto c = 1.0 / win_.count();
        const auto m = cfg.missingValue();
        for (size_t i = 0; i < values_.size(); ++i) {
            if (val[i] == m) { continue; }
            const auto oldMean = mean_[i];
            mean_[i] += c * (val[i] - oldMean);
            values_[i] += (val[i] - oldMean) * (val[i] - mean_[i]);
        }

    }
    void  updateWithMissingAndCounters(const T* val, const StatisticsConfiguration& cfg) {
        const auto m = cfg.missingValue();
        win_.updateCounts(val, values_.size(), m);
        const auto& counts = win_.counts();

        for (size_t i = 0; i < values_.size(); ++i) {
            if (val[i] == m) { continue; }
            const auto c = 1.0 / counts[i];
            const auto oldMean = mean_[i];
            mean_[i] += c * (val[i] - oldMean);
            values_[i] += (val[i] - oldMean) * (val[i] - mean_[i]);
        }
    }

    void print(std::ostream& os) const override { os << logHeader_; }

    std::vector<T> mean_;
};

}  // namespace multio::action::statistics_mtg2
