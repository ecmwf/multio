
#pragma once


#include "eckit/types/DateTime.h"
#include "multio/action/statistics/StatisticsIO.h"
#include "multio/action/statistics/TimeUtils.h"
#include "multio/action/statistics/cfg/StatisticsConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action::statistics {

class PeriodUpdater {
public:
    PeriodUpdater(long span) : span_{span} {};
    PeriodUpdater(const std::string& name, std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) {
        load(name, IOmanager, opt);
    };

    virtual const std::string name() const = 0;

    virtual const std::string timeUnit() const = 0;

    virtual void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const = 0;

    long timeSpan() const { return span_; };

    eckit::DateTime computeWinCreationTime(const eckit::DateTime& currentTime) const { return currentTime; };

    eckit::DateTime computeWinEndTime(const eckit::DateTime& currentTime) const {
        auto ret = eckit::DateTime{updateWinEndTime(currentTime)};
        return ret;
    };

    virtual eckit::DateTime computeWinStartTime(const eckit::DateTime& nextTime) const = 0;

    virtual eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint) const = 0;

protected:
    long span_;

    void baseDump(const std::string& name, std::shared_ptr<StatisticsIO>& IOmanager,
                  const StatisticsOptions& opt) const {
        IOBuffer restartState{IOmanager->getBuffer(restartSize())};
        restartState.zero();
        serialize(restartState, IOmanager->getCurrentDir() + "/" + name + "_dump.txt", opt);
        IOmanager->write(name, static_cast<size_t>(1), restartSize());
        IOmanager->flush();
    };

    void load(const std::string& name, std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) {
        IOBuffer restartState{IOmanager->getBuffer(restartSize())};
        IOmanager->read(name, restartSize());
        deserialize(restartState, IOmanager->getCurrentDir() + "/" + name + "_load.txt", opt);
        restartState.zero();
    };

private:
    size_t restartSize() const { return static_cast<size_t>(2); }

    void serialize(IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt) const {
        if (opt.debugRestart()) {
            std::ofstream outFile(fname);
            // outFile << name() << std::endl;
            // outFile << timeUnit() << std::endl;
            outFile << span_ << std::endl;
            outFile.close();
        }
        currState[0] = static_cast<std::uint64_t>(span_);
        currState.computeChecksum();
    }

    void deserialize(const IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt) {
        currState.checkChecksum();
        span_ = static_cast<long>(currState[0]);
        if (opt.debugRestart()) {
            std::ofstream outFile(fname);
            // outFile << name() << std::endl;
            // outFile << timeUnit() << std::endl;
            outFile << span_ << std::endl;
            outFile.close();
        }
    }
};

}  // namespace multio::action::statistics