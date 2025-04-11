#pragma once

#include <cinttypes>
#include <vector>

#include "eckit/types/DateTime.h"


#include "StatisticsIO.h"
#include "multio/action/statistics/cfg/StatisticsConfiguration.h"
#include "multio/action/statistics/cfg/StatisticsOptions.h"
#include "multio/action/statistics/period-updaters/PeriodUpdater.h"
#include "multio/message/Message.h"
#include "multio/util/DateTime.h"

namespace multio::action::statistics {

enum OperationWindowType : std::int64_t {
    FORWARD_OFFSET  = 0,
    BACKWARD_OFFSET = 1
};

class OperationWindow {
public:
    OperationWindow(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt);

    OperationWindow(const eckit::DateTime& epochPoint, const eckit::DateTime& startPoint,
                    const eckit::DateTime& creationPoint, const eckit::DateTime& endPoint,
                    std::int64_t timeStepInSeconds, OperationWindowType windowType);

    std::int64_t count() const;
    const std::vector<std::int64_t>& counts() const;

    template <typename T>
    void updateCounts(const T* values, std::size_t size, double missingValue) const;

    void updateData(const eckit::DateTime& currentPoint);
    void updateWindow(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const;
    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt);

    std::string windowType() const;
    bool isWithin(const eckit::DateTime& dt) const;
    bool gtLowerBound(const eckit::DateTime& dt, bool throw_error) const;
    bool geLowerBound(const eckit::DateTime& dt, bool throw_error) const;
    bool leUpperBound(const eckit::DateTime& dt, bool throw_error) const;
    bool ltUpperBound(const eckit::DateTime& dt, bool throw_error) const;

    std::int64_t timeSpanInSeconds() const;
    std::int64_t timeSpanInHours() const;
    std::int64_t timeSpanInSteps() const;
    std::int64_t lastPointsDiffInSeconds() const;

    util::DateTimeDiff lastPointsDiff() const;

    std::int64_t startPointInSeconds() const;
    std::int64_t creationPointInSeconds() const;
    std::int64_t currPointInSeconds() const;
    std::int64_t prevPointInSeconds() const;
    std::int64_t endPointInSeconds() const;

    std::int64_t startPointInHours() const;
    std::int64_t creationPointInHours() const;
    std::int64_t currPointInHours() const;
    std::int64_t prevPointInHours() const;
    std::int64_t endPointInHours() const;

    std::int64_t startPointInSteps() const;
    std::int64_t creationPointInSteps() const;
    std::int64_t currPointInSteps() const;
    std::int64_t prevPointInSteps() const;
    std::int64_t endPointInSteps() const;


    std::int64_t startPointInSeconds(const eckit::DateTime& refPoint) const;
    std::int64_t creationPointInSeconds(const eckit::DateTime& refPoint) const;
    std::int64_t currPointInSeconds(const eckit::DateTime& refPoint) const;
    std::int64_t prevPointInSeconds(const eckit::DateTime& refPoint) const;
    std::int64_t endPointInSeconds(const eckit::DateTime& refPoint) const;

    std::int64_t startPointInHours(const eckit::DateTime& refPoint) const;
    std::int64_t creationPointInHours(const eckit::DateTime& refPoint) const;
    std::int64_t currPointInHours(const eckit::DateTime& refPoint) const;
    std::int64_t prevPointInHours(const eckit::DateTime& refPoint) const;
    std::int64_t endPointInHours(const eckit::DateTime& refPoint) const;

    std::int64_t startPointInSteps(const eckit::DateTime& refPoint) const;
    std::int64_t creationPointInSteps(const eckit::DateTime& refPoint) const;
    std::int64_t currPointInSteps(const eckit::DateTime& refPoint) const;
    std::int64_t prevPointInSteps(const eckit::DateTime& refPoint) const;
    std::int64_t endPointInSteps(const eckit::DateTime& refPoint) const;

    std::int64_t timeStepInSeconds() const;


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
    std::int64_t lastFlushInSteps() const;

    std::size_t restartSize() const;

private:
    eckit::DateTime epochPoint_;
    eckit::DateTime startPoint_;
    eckit::DateTime creationPoint_;
    eckit::DateTime currPoint_;
    eckit::DateTime prevPoint_;
    eckit::DateTime endPoint_;
    eckit::DateTime lastFlush_;

    std::int64_t timeStepInSeconds_;
    std::int64_t count_;
    mutable std::vector<std::int64_t> counts_;
    OperationWindowType type_;

    void initCountsLazy(std::size_t size) const;

    void serialize(IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt) const;
    void deserialize(const IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt);

    void print(std::ostream& os) const;
    friend std::ostream& operator<<(std::ostream& os, const OperationWindow& a);
};

OperationWindow make_window(const std::unique_ptr<PeriodUpdater>& periodUpdater, const StatisticsConfiguration& cfg);
OperationWindow load_window(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt);

}  // namespace multio::action::statistics
