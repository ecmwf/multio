
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithDeaccumulatedData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class DeAccumulate final : public OperationWithDeaccumulatedData<T> {
public:
    using OperationWithDeaccumulatedData<T>::name_;
    using OperationWithDeaccumulatedData<T>::cfg_;
    using OperationWithDeaccumulatedData<T>::logHeader_;
    using OperationWithDeaccumulatedData<T>::initValues_;
    using OperationWithDeaccumulatedData<T>::values_;
    using OperationWithDeaccumulatedData<T>::win_;
    using OperationWithDeaccumulatedData<T>::checkSize;
    using OperationWithDeaccumulatedData<T>::checkTimeInterval;


    DeAccumulate(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithDeaccumulatedData<T>{name, "accumulate", sz, true, win, cfg} {}

    DeAccumulate(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                 const StatisticsConfiguration& cfg) :
        OperationWithDeaccumulatedData<T>{name, "accumulate", sz, true, win, IOmanager, cfg} {};

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
        cfg_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        return;
    }

private:
    void computeWithoutMissing(T* val) {
        std::transform(values_.begin(), values_.end(), initValues_.begin(), val,
                       [](T v1, T v2) { return static_cast<T>(v1 - v2); });
        return;
    }

    void computeWithMissing(T* val) {
        double m = cfg_.missingValue();
        std::transform(values_.begin(), values_.end(), initValues_.begin(), val,
                       [m](T v1, T v2) { return static_cast<T>(m == v1 ? m : v1 - v2); });
        return;
    }


    void updateWithoutMissing(const T* val) {
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [](T v1, T v2) { return static_cast<T>(v2); });
        return;
    }

    void updateWithMissing(const T* val) {
        double m = cfg_.missingValue();
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [m](T v1, T v2) { return static_cast<T>(m == v2 ? m : v2); });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
