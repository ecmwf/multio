
#pragma once

#include "multio/action/statistics/operations/Operation.h"
#include "multio/action/statistics/TimeUtils.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithDeaccumulatedData : public Operation {
public:
    using Operation::cfg_;
    using Operation::logHeader_;
    using Operation::name_;

    OperationWithDeaccumulatedData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                                   const OperationWindow& win, const StatisticsConfiguration& cfg) :
        Operation{name, operation, win, cfg},
        values_{std::vector<T>(sz / sizeof(T), 0.0)},
        initValues_{std::vector<T>(sz / sizeof(T), 0.0)},
        needRestart_{needRestart} {}

    OperationWithDeaccumulatedData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                                   const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                                   const StatisticsConfiguration& cfg) :
        Operation{name, operation, win, cfg},
        values_{std::vector<T>(sz / sizeof(T), 0.0)},
        initValues_{std::vector<T>(sz / sizeof(T), 0.0)},
        needRestart_{needRestart} {
        load(IOmanager, cfg);
        return;
    }

    void updateWindow(const void* data, long sz, const message::Message& msg, const StatisticsConfiguration& cfg) override {
        checkSize(sz);
        if ( solverResetAccumulatedFields(msg, cfg) ) {
            std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                           [](const T& v1) { return static_cast<T>(0); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        } else{
            const T* val = static_cast<const T*>(data);
            std::transform(initValues_.begin(), initValues_.end(), val, initValues_.begin(),
                           [](const T& v1, const T& v2) { return static_cast<T>(v2); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
        return;
    };

    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                       [](const T& v1) { return static_cast<T>(0); });
        std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        return;
    };

    void init(const void* data, long sz, const message::Message& msg, const StatisticsConfiguration& cfg) override {
        checkSize(sz);
        if ( solverResetAccumulatedFields(msg, cfg) ) {
            std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                           [](const T& v1) { return static_cast<T>(0); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        } else{
            const T* val = static_cast<const T*>(data);
            std::transform(initValues_.begin(), initValues_.end(), val, initValues_.begin(),
                           [](const T& v1, const T& v2) { return static_cast<T>(v2); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
        return;
    };

    void init(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                           [](const T& v1) { return static_cast<T>(0); });
        std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        return;
    };

    bool needStepZero() const override { return true; };

    size_t byte_size() const override { return values_.size() * sizeof(T); };

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const override {
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            restartState.zero();
            serialize(restartState);
            IOmanager->write(name_, restartSize());
            IOmanager->flush();
        }
        return;
    };

    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) override {
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            IOmanager->read(name_, restartSize());
            deserialize(restartState);
            restartState.zero();
        }
        return;
    };

protected:
    void serialize(IOBuffer& restartState) const {
        size_t sz = values_.size();
        for (size_t i = 0; i < sz; ++i) {
            T lv = initValues_[i];
            double dv = static_cast<double>(lv);
            restartState[i] = *reinterpret_cast<uint64_t*>(&dv);
        }
        for (size_t i = 0; i < sz; ++i) {
            T lv = values_[i];
            double dv = static_cast<double>(lv);
            restartState[sz + i] = *reinterpret_cast<uint64_t*>(&dv);
        }
        restartState.computeChecksum();
        return;
    };

    void deserialize(const IOBuffer& restartState) {
        restartState.checkChecksum();
        size_t sz = values_.size();
        for (size_t i = 0; i < sz; ++i) {
            std::uint64_t lv = restartState[i];
            double dv = *reinterpret_cast<double*>(&lv);
            initValues_[i] = static_cast<T>(dv);
        }
        for (size_t i = 0; i < sz; ++i) {
            std::uint64_t lv = restartState[sz + i];
            double dv = *reinterpret_cast<double*>(&lv);
            values_[i] = static_cast<T>(dv);
        }
        return;
    };

    void checkSize(long sz) {
        if (values_.size() != static_cast<long>(sz / sizeof(T))) {
            throw eckit::AssertionFailed(logHeader_ + " :: Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }
    };

    void checkTimeInterval() {
        long sec = win_.count() * cfg_.stepFreq() * cfg_.timeStep();
        if (sec == 0) {
            throw eckit::SeriousBug{logHeader_ + " :: Divide by zero", Here()};
        }
        return;
    };

    size_t restartSize() const { return 2 * values_.size() + 1; }
    std::vector<T> values_;
    std::vector<T> initValues_;

private:
    bool needRestart_;

    bool solverResetAccumulatedFields(const message::Message& msg, const StatisticsConfiguration& cfg) {

        if (cfg.solverResetAccumulatedFields() == "hour") {
            return isBeginningOfHour(msg, cfg);
        }
        if (cfg.solverResetAccumulatedFields() == "day") {
            return isBeginningOfDay(msg, cfg);
        }
        if (cfg.solverResetAccumulatedFields() == "month") {
            return isBeginningOfMonth(msg, cfg);
        }
        if (cfg.solverResetAccumulatedFields() == "year") {
            return isBeginningOfYear(msg, cfg);
        }
        if (cfg.solverResetAccumulatedFields() == "never") {
            return false;
        }

        std::ostringstream os;
        os << "Invalid reset period of accumulated fields :: " << cfg.solverResetAccumulatedFields() << std::endl;
        throw eckit::UserError(os.str(), Here());
    }

};

}  // namespace multio::action
