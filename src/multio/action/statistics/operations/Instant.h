
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point_v<T>>>
class Instant final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;


    Instant(const std::string& name, std::size_t size, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{name, "instant", size, true, win, cfg} {}

    Instant(const std::string& name, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsOptions& opt) :
        OperationWithData<T>{name, "instant", true, win, IOmanager, opt} {};

    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) override {
        checkTimeInterval(cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        buf.copy(values_.data(), values_.size() * sizeof(T));
    }

    void updateData(const void* data, std::size_t size, const StatisticsConfiguration& cfg) override {
        checkSize(size, cfg);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        std::copy(val, val + (size / sizeof(T)), values_.begin());
    }

private:
    void print(std::ostream& os) const override { os << logHeader_; }
};
}  // namespace multio::action
