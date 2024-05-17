
#pragma once

#include "multio/action/statistics/operations/Operation.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithData : public Operation {
public:
    using Operation::cfg_;
    using Operation::logHeader_;
    using Operation::name_;

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const OperationWindow& win, const StatisticsConfiguration& cfg, T initial_value = 0.0) :
        Operation{name, operation, win, cfg},
        values_{std::vector<T>(sz /= sizeof(T), initial_value)},
        needRestart_{needRestart}, initialValue_{initial_value} {}

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                      const StatisticsConfiguration& cfg, T initial_value = 0.0) :
        Operation{name, operation, win, cfg}, values_{std::vector<T>(sz /= sizeof(T), initial_value)}, needRestart_{needRestart},
        initialValue_{initial_value}
    {
        load(IOmanager, cfg);
    }

    void updateWindow(const void* data, long sz, const message::Message& msg,
                      const StatisticsConfiguration& cfg) override {
        std::fill(values_.begin(), values_.end(), initialValue_);
    };

    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        std::fill(values_.begin(), values_.end(), initialValue_);
    };

    void init(const void* data, long sz, const message::Message& msg, const StatisticsConfiguration& cfg) override {
        // TODO: Used to save the first field of the window
        return;
    };

    void init(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        // TODO: Used to save the initialization time of the window
        return;
    };

    bool needStepZero() const override { return false; };

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
        std::transform(values_.cbegin(), values_.cend(), restartState.begin(), [](const T& v) {
            T lv = v;
            double dv = static_cast<double>(lv);
            return *reinterpret_cast<uint64_t*>(&dv);
        });
        restartState.computeChecksum();
        return;
    };

    void deserialize(const IOBuffer& restartState) {
        restartState.checkChecksum();
        auto last = restartState.cend();
        std::transform(restartState.cbegin(), --last, values_.begin(), [](const std::uint64_t& v) {
            std::uint64_t lv = v;
            double dv = *reinterpret_cast<double*>(&lv);
            return static_cast<T>(dv);
        });
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

    size_t restartSize() const { return values_.size() + 1; }
    std::vector<T> values_;

private:
    bool needRestart_;
    const T initialValue_;
};

}  // namespace multio::action
