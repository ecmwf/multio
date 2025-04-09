
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithDeaccumulatedData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class Difference final : public OperationWithDeaccumulatedData<T> {
public:
    using OperationWithDeaccumulatedData<T>::name_;
    using OperationWithDeaccumulatedData<T>::logHeader_;
    using OperationWithDeaccumulatedData<T>::initValues_;
    using OperationWithDeaccumulatedData<T>::values_;
    using OperationWithDeaccumulatedData<T>::win_;
    using OperationWithDeaccumulatedData<T>::checkSize;
    using OperationWithDeaccumulatedData<T>::checkTimeInterval;


    Difference(const std::string& name, std::size_t size, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithDeaccumulatedData<T>{name, "difference", size, true, win, cfg} {}

    Difference(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
               const StatisticsOptions& opt) :
        OperationWithDeaccumulatedData<T>{name, "difference", true, win, IOmanager, opt} {};

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
    void computeWithoutMissing(T* val, const StatisticsConfiguration& cfg) {
        std::transform(values_.begin(), values_.end(), initValues_.begin(), val,
                       [](T v1, T v2) { return static_cast<T>(v1 - v2); });
    }

    void computeWithMissing(T* val, const StatisticsConfiguration& cfg) {
        const auto m = cfg.missingValue();
        std::transform(values_.begin(), values_.end(), initValues_.begin(), val,
                       [m](T v1, T v2) { return static_cast<T>(m == v1 || m == v2 ? m : v1 - v2); });
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
