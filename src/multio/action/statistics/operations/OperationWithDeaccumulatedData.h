
#pragma once

#include "multio/action/statistics/TimeUtils.h"
#include "multio/action/statistics/operations/Operation.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithDeaccumulatedData : public Operation {
public:
    using Operation::logHeader_;
    using Operation::name_;

    OperationWithDeaccumulatedData(const std::string& name, const std::string& operation, std::size_t size, bool needRestart,
                                   const OperationWindow& win, const StatisticsConfiguration& cfg) :
        Operation{name, operation, win, cfg.options()},
        values_{std::vector<T>(size / sizeof(T), 0.0)},
        initValues_{std::vector<T>(size / sizeof(T), 0.0)},
        needRestart_{needRestart} {}

    OperationWithDeaccumulatedData(const std::string& name, const std::string& operation, bool needRestart,
                                   const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                                   const StatisticsOptions& opt) :
        Operation{name, operation, win, opt}, values_{}, initValues_{}, needRestart_{needRestart} {
        load(IOmanager, opt);
    }

    void updateWindow(const void* data, std::size_t size, const message::Message& msg,
                      const StatisticsConfiguration& cfg) override {
        checkSize(size, cfg);
        if (solverResetAccumulatedFields(msg, cfg)) {
            std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                           [](const T& v1) { return static_cast<T>(0.0); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
        else {
            const auto val = static_cast<const T*>(data);
            std::transform(initValues_.begin(), initValues_.end(), val, initValues_.begin(),
                           [](const T& v1, const T& v2) { return static_cast<T>(v2); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
    };

    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                       [](const T& v1) { return static_cast<T>(0.0); });
        std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
    };

    void init(const void* data, std::size_t size, const message::Message& msg, const StatisticsConfiguration& cfg) override {
        checkSize(size, cfg);
        if (solverResetAccumulatedFields(msg, cfg)) {
            std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                           [](const T& v1) { return static_cast<T>(0.0); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
        else {
            const auto val = static_cast<const T*>(data);
            std::transform(initValues_.begin(), initValues_.end(), val, initValues_.begin(),
                           [](const T& v1, const T& v2) { return static_cast<T>(v2); });
            std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        }
    };

    void init(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        std::transform(initValues_.begin(), initValues_.end(), initValues_.begin(),
                       [](const T& v1) { return static_cast<T>(0.0); });
        std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
    };

    bool needStepZero() const override { return true; };

    std::size_t byte_size() const override { return values_.size() * sizeof(T); };

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const override {
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            restartState.zero();
            auto fname = restartFileName();
            serialize(restartState, IOmanager->getCurrentDir() + "/" + fname + "_dump.txt", opt);
            IOmanager->write(fname, values_.size(), restartSize());
            IOmanager->flush();
        }
    };

    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) override {
        if (needRestart_) {
            std::size_t size;
            auto fname = restartFileName();
            IOmanager->readSize(fname, size);
            values_.resize(size);
            initValues_.resize(size);
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            IOmanager->read(fname, restartSize());
            deserialize(restartState, IOmanager->getCurrentDir() + "/" + fname + "_load.txt", opt);
            restartState.zero();
        }
    };

protected:
    void serialize(IOBuffer& restartState, const std::string& fname, const StatisticsOptions& opt) const {
        auto size = values_.size();
        std::size_t count = 0;
        for (std::size_t i = 0; i < size; ++i) {
            auto lv = initValues_[i];
            auto dv = static_cast<double>(lv);
            restartState[count++] = *reinterpret_cast<std::uint64_t*>(&dv);
        }
        for (std::size_t i = 0; i < size; ++i) {
            auto lv = values_[i];
            auto dv = static_cast<double>(lv);
            restartState[count++] = *reinterpret_cast<std::uint64_t*>(&dv);
        }
        restartState.computeChecksum();
        // debug restart
        if (opt.debugRestart()) {
            std::ofstream outFile(fname);
            outFile << "initValues(" << size << ")" << std::endl;
            for (std::size_t i = 0; i < size; ++i) {
                outFile << i << ", " << initValues_[i] << std::endl;
            }
            outFile << "values(" << size << ")" << std::endl;
            for (std::size_t i = 0; i < size; ++i) {
                outFile << i << ", " << values_[i] << std::endl;
            }
            outFile.close();
        }
    };

    void deserialize(const IOBuffer& restartState, const std::string& fname, const StatisticsOptions& opt) {
        restartState.checkChecksum();
        std::size_t count = 0;
        auto size = values_.size();
        for (std::size_t i = 0; i < size; ++i) {
            auto lv = restartState[count++];
            auto dv = *reinterpret_cast<double*>(&lv);
            initValues_[i] = static_cast<T>(dv);
        }
        for (std::size_t i = 0; i < size; ++i) {
            auto lv = restartState[count++];
            auto dv = *reinterpret_cast<double*>(&lv);
            values_[i] = static_cast<T>(dv);
        }
        // debug restart
        if (opt.debugRestart()) {
            std::ofstream outFile(fname);
            outFile << "initValues(" << size << ")" << std::endl;
            for (std::size_t i = 0; i < size; ++i) {
                outFile << i << ", " << initValues_[i] << std::endl;
            }
            outFile << "values(" << size << ")" << std::endl;
            for (std::size_t i = 0; i < size; ++i) {
                outFile << i << ", " << values_[i] << std::endl;
            }
            outFile.close();
        }
    };

    void checkSize(std::size_t size, const StatisticsConfiguration& cfg) {
        if (values_.size() != size / sizeof(T)) {
            throw eckit::AssertionFailed(logHeader_ + " :: Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(size));
        }
    };

    void checkTimeInterval(const StatisticsConfiguration& cfg) {
        auto sec = win_.count() * cfg.stepFreq() * cfg.timeStep();
        if (sec == 0) {
            throw eckit::SeriousBug{logHeader_ + " :: Divide by zero", Here()};
        }
    };

    std::size_t restartSize() const { return 2 * values_.size() + 1; }
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
        os << "Invalid reset period of accumulated fields :: " << cfg.options().solverResetAccumulatedFields()
           << std::endl;
        throw eckit::UserError(os.str(), Here());
    }
};

}  // namespace multio::action
