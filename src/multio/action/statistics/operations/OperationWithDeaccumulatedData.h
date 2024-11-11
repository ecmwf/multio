
#pragma once

#include "multio/action/statistics/TimeUtils.h"
#include "multio/action/statistics/operations/Operation.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithDeaccumulatedData : public Operation {
public:
    using Operation::logHeader_;
    using Operation::name_;

    OperationWithDeaccumulatedData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                                   const OperationWindow& win, const StatisticsConfiguration& cfg) :
        Operation{name, operation, win, cfg.options()},
        values_{std::vector<T>(sz / sizeof(T), 0.0)},
        initValues_{std::vector<T>(sz / sizeof(T), 0.0)},
        needRestart_{needRestart} {}

    OperationWithDeaccumulatedData(const std::string& name, const std::string& operation, bool needRestart,
                                   const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                                    const StatisticsOptions& opt) :
        Operation{name, operation, win, opt},
        values_{},
        initValues_{},
        needRestart_{needRestart} {
        load(IOmanager, opt);
        return;
    }

    void updateWindow(const void* data, long sz, const message::Message& msg, const StatisticsConfiguration& cfg) override {
        checkSize(sz,cfg);
        if (solverResetAccumulatedFields(msg, cfg)) {
            std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                           [](const T& v1) { return static_cast<T>(0.0); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
        else {
            const T* val = static_cast<const T*>(data);
            std::transform(initValues_.begin(), initValues_.end(), val, initValues_.begin(),
                           [](const T& v1, const T& v2) { return static_cast<T>(v2); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
        return;
    };

    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                       [](const T& v1) { return static_cast<T>(0.0); });
        std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        return;
    };

    void init(const void* data, long sz, const message::Message& msg, const StatisticsConfiguration& cfg) override {
        checkSize(sz,cfg);
        if (solverResetAccumulatedFields(msg, cfg)) {
            std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                           [](const T& v1) { return static_cast<T>(0.0); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
        else {
            const T* val = static_cast<const T*>(data);
            std::transform(initValues_.begin(), initValues_.end(), val, initValues_.begin(),
                           [](const T& v1, const T& v2) { return static_cast<T>(v2); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
        return;
    };

    void init(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                       [](const T& v1) { return static_cast<T>(0.0); });
        std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        return;
    };

    bool needStepZero() const override { return true; };

    size_t byte_size() const override { return values_.size() * sizeof(T); };

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const override {
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            restartState.zero();
            std::string fname = restartFileName();
            serialize(restartState, IOmanager->getCurrentDir() + "/" + fname + "_dump.txt", opt);
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
            initValues_.resize(sz);
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            IOmanager->read(fname, restartSize());
            deserialize(restartState, IOmanager->getCurrentDir() + "/" + fname + "_load.txt", opt);
            restartState.zero();
        }
        return;
    };

protected:
    void serialize(IOBuffer& restartState, const std::string& fname, const StatisticsOptions& opt) const {
        size_t sz = values_.size();
        size_t cnt=0;
        // restartState[cnt] = static_cast<uint64_t>(sz);
        for (size_t i = 0; i < sz; ++i) {
            T lv = initValues_[i];
            double dv = static_cast<double>(lv);
            restartState[cnt] = *reinterpret_cast<uint64_t*>(&dv);
            cnt++;
        }
        for (size_t i = 0; i < sz; ++i) {
            T lv = values_[i];
            double dv = static_cast<double>(lv);
            restartState[cnt] = *reinterpret_cast<uint64_t*>(&dv);
            cnt++;
        }
        restartState.computeChecksum();
        // debug restart
        if (opt.debugRestart()) {
            std::ofstream outFile(fname);
            outFile << "initValues(" << sz << ")" << std::endl;
            for (size_t i = 0; i < sz; ++i) {
                outFile <<  i << ", " << initValues_[i] << std::endl;
            }
            outFile << "values(" << sz << ")" << std::endl;
            for (size_t i = 0; i < sz; ++i) {
                outFile <<  i << ", " << values_[i] << std::endl;
            }
            outFile.close();
        }
        return;
    };

    void deserialize(const IOBuffer& restartState, const std::string& fname, const StatisticsOptions& opt) {
        restartState.checkChecksum();
        size_t cnt=0;
        size_t sz = values_.size();
        // size_t sz = static_cast<size_t>(restartState[cnt]);
        for (size_t i = 0; i < sz; ++i) {
            std::uint64_t lv = restartState[cnt];
            double dv = *reinterpret_cast<double*>(&lv);
            initValues_[i] = static_cast<T>(dv);
            cnt++;
        }
        for (size_t i = 0; i < sz; ++i) {
            std::uint64_t lv = restartState[cnt];
            double dv = *reinterpret_cast<double*>(&lv);
            values_[i] = static_cast<T>(dv);
            cnt++;
        }
        // debug restart
        if (opt.debugRestart()) {
            std::ofstream outFile(fname);
            outFile << "initValues(" << sz << ")" << std::endl;
            for (size_t i = 0; i < sz; ++i) {
                outFile <<  i << ", " << initValues_[i] << std::endl;
            }
            outFile << "values(" << sz << ")" << std::endl;
            for (size_t i = 0; i < sz; ++i) {
                outFile <<  i << ", " << values_[i] << std::endl;
            }
            outFile.close();
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

    size_t restartSize() const { return 2 * values_.size() + 1; }
    std::vector<T> values_;
    std::vector<T> initValues_;

private:
    bool needRestart_;

    const std::string restartFileName() const { return name_ + "_" + (sizeof(T) == 4 ? "single" : "double"); };

    bool solverResetAccumulatedFields(const message::Message& msg, const StatisticsConfiguration& cfg) {

        if (cfg.options().solverResetAccumulatedFields() == "hour") {
            return isBeginningOfHour(msg, cfg);
        }
        if (cfg.options().solverResetAccumulatedFields() == "day") {
            return isBeginningOfDay(msg, cfg);
        }
        if (cfg.options().solverResetAccumulatedFields() == "month") {
            return isBeginningOfMonth(msg, cfg);
        }
        if (cfg.options().solverResetAccumulatedFields() == "year") {
            return isBeginningOfYear(msg, cfg);
        }
        if (cfg.options().solverResetAccumulatedFields() == "never") {
            return false;
        }

        std::ostringstream os;
        os << "Invalid reset period of accumulated fields :: " << cfg.options().solverResetAccumulatedFields() << std::endl;
        throw eckit::UserError(os.str(), Here());
    }
};

}  // namespace multio::action
