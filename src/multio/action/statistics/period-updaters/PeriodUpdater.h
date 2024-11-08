
#pragma once


#include "multio/action/statistics/cfg/StatisticsConfiguration.h"
#include "multio/action/statistics/TimeUtils.h"
#include "eckit/types/DateTime.h"
#include "multio/message/Message.h"
#include "multio/action/statistics/StatisticsIO.h"


namespace multio::action {

class PeriodUpdater {
public:

    PeriodUpdater(long span) : span_{span} {};
    PeriodUpdater( const std::string& name, std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) {
        load(name, IOmanager, opt);
    };

    virtual const std::string name() const = 0;

    virtual const std::string timeUnit() const = 0;

    virtual void dump( std::shared_ptr<StatisticsIO>& IOmanager,  const StatisticsOptions& opt ) const = 0;

    long timeSpan() const {
        return span_;
    };

    eckit::DateTime computeWinCreationTime(const eckit::DateTime& currentTime) const {
        return currentTime;
    };

    eckit::DateTime computeWinEndTime(const eckit::DateTime& currentTime) const {
        auto ret = eckit::DateTime{updateWinEndTime(currentTime)};
        return ret;
    };

    virtual eckit::DateTime computeWinStartTime(const eckit::DateTime& nextTime) const = 0;

    virtual eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint) const = 0;

protected:
    long span_;

    void baseDump( const std::string& name, std::shared_ptr<StatisticsIO>& IOmanager,  const StatisticsOptions& opt ) const {
        IOBuffer restartState{IOmanager->getBuffer(restartSize())};
        restartState.zero();
        serialize(restartState);
        IOmanager->write(name, static_cast<size_t>(1), restartSize() );
        IOmanager->flush();
        return;
    };

    void load( const std::string& name, std::shared_ptr<StatisticsIO>& IOmanager,  const StatisticsOptions& opt ) {
        IOBuffer restartState{IOmanager->getBuffer(restartSize())};
        IOmanager->read( name, restartSize() );
        deserialize(restartState);
        restartState.zero();
        return;
    };

private:

    size_t restartSize() const {
        return static_cast<size_t>(2);
    }

    void serialize(IOBuffer& currState) const {
        currState[0] = static_cast<std::uint64_t>(span_);
        currState.computeChecksum();
        return;
    }

    void deserialize(const IOBuffer& currState) {
        currState.checkChecksum();
        span_ = static_cast<long>(currState[0]);
        return;
    }

};

}