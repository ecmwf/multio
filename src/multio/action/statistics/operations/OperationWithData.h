
#pragma once

#include "multio/action/statistics/operations/Operation.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithData : public Operation {
public:
    using Operation::logHeader_;
    using Operation::name_;

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const OperationWindow& win, const StatisticsConfiguration& cfg, T initial_value = 0.0) :
        Operation{name, operation, win, cfg.options()},
        values_{std::vector<T>(sz /= sizeof(T), initial_value)},
        needRestart_{needRestart},
        initialValue_{initial_value} {}

    OperationWithData(const std::string& name, const std::string& operation, bool needRestart,
                      const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                      const StatisticsOptions& opt, T initial_value = 0.0) :
        Operation{name, operation, win, opt},
        values_{},
        needRestart_{needRestart},
        initialValue_{initial_value} {
        load(IOmanager, opt);
    }

    void updateWindow(const void* data, long sz, const message::Message& msg, const StatisticsConfiguration& cfg) override {
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

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const override {
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            restartState.zero();
            serialize(restartState);
            std::string fname = restartFileName();
            IOmanager->write(fname, values_.size(), restartSize());
            IOmanager->flush();
        }
        return;
    };

    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) override {
        if (needRestart_) {
            std::uint64_t sz;
            std::string fname = restartFileName();
            IOmanager->readSize(fname, sz);
            values_.resize(sz);
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            IOmanager->read(fname, restartSize());
            deserialize(restartState);
            restartState.zero();
        }
        return;
    };

protected:
    void serialize(IOBuffer& restartState) const {

        size_t sz = values_.size();
        size_t cnt=0;
        // restartState[cnt] = static_cast<uint64_t>(sz);
        for (size_t i = 0; i < sz; ++i) {
            T lv = values_[i];
            double dv = static_cast<double>(lv);
            restartState[cnt] = *reinterpret_cast<uint64_t*>(&dv);
            cnt++;
        }
        restartState.computeChecksum();
        return;
    };

    void deserialize(const IOBuffer& restartState) {
        restartState.checkChecksum();
        size_t cnt=0;
        size_t sz = values_.size();
        for (size_t i = 0; i < sz; ++i) {
            std::uint64_t lv = restartState[cnt];
            double dv = *reinterpret_cast<double*>(&lv);
            values_[i] = static_cast<T>(dv);
            cnt++;
        }
        return;
    };

    void checkSize(long sz, const StatisticsConfiguration& cfg) {
        if (values_.size() != static_cast<long>(sz / sizeof(T))) {
            throw eckit::AssertionFailed(logHeader_ + " :: Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }
    };

    void checkTimeInterval( const StatisticsConfiguration& cfg ) {
        long sec = win_.count() * cfg.stepFreq() * cfg.timeStep();
        if (sec == 0) {
            throw eckit::SeriousBug{logHeader_ + " :: Divide by zero", Here()};
        }
        return;
    };

    size_t restartSize() const { return values_.size() + 1; }
    std::vector<T> values_;

private:

    const std::string restartFileName() const { return name_ + "_" + (sizeof(T) == 4 ? "single" : "double"); };

    bool needRestart_;
    const T initialValue_;
};

}  // namespace multio::action
