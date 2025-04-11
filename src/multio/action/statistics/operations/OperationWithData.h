
#pragma once

#include "multio/action/statistics/operations/Operation.h"

namespace multio::action::statistics {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithData : public Operation {
public:
    using Operation::logHeader_;
    using Operation::name_;

    OperationWithData(const std::string& name, const std::string& operation, std::size_t size, bool needRestart,
                      const OperationWindow& win, const StatisticsConfiguration& cfg, T initial_value = 0.0) :
        Operation{name, operation, win, cfg.options()},
        values_{std::vector<T>(size /= sizeof(T), initial_value)},
        needRestart_{needRestart},
        initialValue_{initial_value} {}

    OperationWithData(const std::string& name, const std::string& operation, bool needRestart,
                      const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                      const StatisticsOptions& opt, T initial_value = 0.0) :
        Operation{name, operation, win, opt}, values_{}, needRestart_{needRestart}, initialValue_{initial_value} {
        load(IOmanager, opt);
    }

    void updateWindow(const void* data, std::size_t size, const message::Message& msg,
                      const StatisticsConfiguration& cfg) override {
        std::fill(values_.begin(), values_.end(), initialValue_);
    };

    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        std::fill(values_.begin(), values_.end(), initialValue_);
    };

    void init(const void* data, std::size_t size, const message::Message& msg, const StatisticsConfiguration& cfg) override {
        // TODO: Used to save the first field of the window
    };

    void init(const message::Message& msg, const StatisticsConfiguration& cfg) override {
        // TODO: Used to save the initialization time of the window
    };

    bool needStepZero() const override { return false; };

    std::size_t byte_size() const override { return values_.size() * sizeof(T); };

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const override {
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            restartState.zero();
            std::string fname = restartFileName();
            serialize(restartState, IOmanager->getCurrentDir() + "/" + fname + "_dump.txt", opt);
            IOmanager->write(fname, values_.size(), restartSize());
            IOmanager->flush();
        }
    };

    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) override {
        if (needRestart_) {
            std::size_t size;
            std::string fname = restartFileName();
            IOmanager->readSize(fname, size);
            values_.resize(size);
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            IOmanager->read(fname, restartSize());
            deserialize(restartState, IOmanager->getCurrentDir() + "/" + fname + "_load.txt", opt);
            restartState.zero();
        }
    };

protected:
    void serialize(IOBuffer& restartState, const std::string& fname, const StatisticsOptions& opt) const {

        const auto size = values_.size();
        for (std::size_t i = 0; i < size; ++i) {
            auto lv = values_[i];
            auto dv = static_cast<double>(lv);
            restartState[i] = *reinterpret_cast<std::uint64_t*>(&dv);
        }
        restartState.computeChecksum();

        if (opt.debugRestart()) {
            std::ofstream outFile(fname);
            outFile << "values(" << size << ")" << std::endl;
            for (std::size_t i = 0; i < size; ++i) {
                outFile << i << ", " << values_[i] << std::endl;
            }
            outFile.close();
        }
    };

    void deserialize(const IOBuffer& restartState, const std::string& fname, const StatisticsOptions& opt) {
        restartState.checkChecksum();
        auto size = values_.size();
        for (std::size_t i = 0; i < size; ++i) {
            auto lv = restartState[i];
            auto dv = *reinterpret_cast<double*>(&lv);
            values_[i] = static_cast<T>(dv);
        }
        if (opt.debugRestart()) {
            std::ofstream outFile(fname);
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

    std::size_t restartSize() const { return values_.size() + 1; }
    std::vector<T> values_;

private:
    const std::string restartFileName() const { return name_ + "_" + (sizeof(T) == 4 ? "single" : "double"); };

    bool needRestart_;
    const T initialValue_;
};

}  // namespace multio::action::statistics
