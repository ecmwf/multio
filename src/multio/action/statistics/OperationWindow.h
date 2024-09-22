#pragma once

#include <cinttypes>
#include <vector>

#include "eckit/types/DateTime.h"


#include "multio/action/statistics/cfg/StatisticsOptions.h"
#include "multio/action/statistics/cfg/StatisticsConfiguration.h"
#include "multio/action/statistics/periodUpdaters/PeriodUpdater.h"
#include "StatisticsIO.h"
#include "multio/message/Message.h"
#include "multio/util/DateTime.h"

namespace multio::action {

class OperationWindow {
public:
    OperationWindow(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt);

    OperationWindow(const eckit::DateTime& epochPoint, const eckit::DateTime& startPoint,
                    const eckit::DateTime& creationPoint, const eckit::DateTime& endPoint, long timeStepInSeconds);

    long count() const;

    void updateData(const eckit::DateTime& currentPoint);
    void updateWindow(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const;
    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt );

    bool isWithin(const eckit::DateTime& dt) const;
    bool gtLowerBound(const eckit::DateTime& dt, bool throw_error) const;
    bool leUpperBound(const eckit::DateTime& dt, bool throw_error) const;

    long timeSpanInSeconds() const;
    long timeSpanInHours() const;
    long timeSpanInSteps() const;
    long lastPointsDiffInSeconds() const;

    util::DateTimeDiff lastPointsDiff() const;

    long startPointInSeconds() const;
    long creationPointInSeconds() const;
    long currPointInSeconds() const;
    long prevPointInSeconds() const;
    long endPointInSeconds() const;

    long startPointInHours() const;
    long creationPointInHours() const;
    long currPointInHours() const;
    long prevPointInHours() const;
    long endPointInHours() const;

    long startPointInSteps() const;
    long creationPointInSteps() const;
    long currPointInSteps() const;
    long prevPointInSteps() const;
    long endPointInSteps() const;


    long startPointInSeconds(const eckit::DateTime& refPoint) const;
    long creationPointInSeconds(const eckit::DateTime& refPoint) const;
    long currPointInSeconds(const eckit::DateTime& refPoint) const;
    long prevPointInSeconds(const eckit::DateTime& refPoint) const;
    long endPointInSeconds(const eckit::DateTime& refPoint) const;

    long startPointInHours(const eckit::DateTime& refPoint) const;
    long creationPointInHours(const eckit::DateTime& refPoint) const;
    long currPointInHours(const eckit::DateTime& refPoint) const;
    long prevPointInHours(const eckit::DateTime& refPoint) const;
    long endPointInHours(const eckit::DateTime& refPoint) const;

    long startPointInSteps(const eckit::DateTime& refPoint) const;
    long creationPointInSteps(const eckit::DateTime& refPoint) const;
    long currPointInSteps(const eckit::DateTime& refPoint) const;
    long prevPointInSteps(const eckit::DateTime& refPoint) const;
    long endPointInSteps(const eckit::DateTime& refPoint) const;

    long timeStepInSeconds() const;


    eckit::DateTime epochPoint() const;
    eckit::DateTime startPoint() const;
    eckit::DateTime creationPoint() const;
    eckit::DateTime currPoint() const;
    eckit::DateTime prevPoint() const;
    eckit::DateTime endPoint() const;

    std::string stepRange() const;
    std::string stepRangeInHours() const;

    std::string stepRange(const eckit::DateTime& refPoint) const;
    std::string stepRangeInHours(const eckit::DateTime& refPoint) const;

    void updateFlush();
    long lastFlushInSteps() const;

    size_t restartSize() const;

private:
    eckit::DateTime epochPoint_;
    eckit::DateTime startPoint_;
    eckit::DateTime creationPoint_;
    eckit::DateTime currPoint_;
    eckit::DateTime prevPoint_;
    eckit::DateTime endPoint_;
    eckit::DateTime lastFlush_;

    long timeStepInSeconds_;
    long count_;

    void serialize(IOBuffer& currState) const;
    void deserialize(const IOBuffer& currState);

    void print(std::ostream& os) const;
    friend std::ostream& operator<<(std::ostream& os, const OperationWindow& a);
};

OperationWindow make_window( const std::unique_ptr<PeriodUpdater>& periodUpdater, const StatisticsConfiguration& cfg);
OperationWindow load_window( std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt );

}  // namespace multio::action

