#pragma once

#include <cinttypes>
#include <map>
#include <optional>
#include <vector>

#include "eckit/types/DateTime.h"


#include "StatisticsIO.h"
#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"
#include "multio/action/statistics-mtg2/cfg/StatisticsOptions.h"
#include "multio/action/statistics-mtg2/period-updaters/PeriodUpdater.h"
#include "multio/message/Message.h"
#include "multio/util/DateTime.h"

namespace multio::action::statistics_mtg2 {

class OperationWindow {
public:
    OperationWindow(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt);

    OperationWindow(const eckit::DateTime& epochPoint, const eckit::DateTime& startPoint,
                    const eckit::DateTime& creationPoint, const eckit::DateTime& endPoint, WindowType windowType,
                    WindowOrigin windowOrigin = WindowOrigin::WindowStart);

    long count() const;
    const std::vector<long>& counts() const;

    template <typename T>
    void updateCounts(const T* values, size_t size, double missingValue) const;

    void updateData(const eckit::DateTime& currentPoint, std::int64_t distanceFromPreviousStepInSeconds);
    void updateWindow(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);

    /// Forecast-start windows only: move the end to the next emission point without touching the accumulated state
    void extendEnd(const eckit::DateTime& endPoint);

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const;
    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt);

    bool isWithin(const eckit::DateTime& dt) const;
    bool gtLowerBound(const eckit::DateTime& dt, bool throw_error) const;
    bool geLowerBound(const eckit::DateTime& dt, bool throw_error) const;
    bool leUpperBound(const eckit::DateTime& dt, bool throw_error) const;
    bool ltUpperBound(const eckit::DateTime& dt, bool throw_error) const;

    long timeSpanInSeconds() const;
    long timeSpanInHours() const;
    /// Length reported with an emission: the nominal window length, or for a forecast-start window the elapsed
    /// length from the window start to the current point
    long emissionSpanInSeconds() const;
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

    eckit::DateTime epochPoint() const;
    eckit::DateTime startPoint() const;
    eckit::DateTime creationPoint() const;
    eckit::DateTime currPoint() const;
    eckit::DateTime prevPoint() const;
    eckit::DateTime endPoint() const;

    bool isComplete() const;
    bool isUniform() const;
    std::string incompleteReason() const;
    WindowType windowType() const;
    WindowOrigin windowOrigin() const;
    /// Forecast-start windows only: samples have been received since the last emission
    bool hasUnemittedSamples() const;
    std::int64_t lastDeclaredDistance() const;
    const std::map<std::int64_t, std::size_t>& declaredDistanceHistogram() const;
    std::map<std::int64_t, std::size_t> observedDistanceHistogram() const;

    std::string stepRangeInHours() const;

    std::string stepRangeInHours(const eckit::DateTime& refPoint) const;

    /// Records the current point as the last emitted point
    void updateFlush();

    size_t restartSize() const;

private:
    eckit::DateTime epochPoint_;
    eckit::DateTime startPoint_;
    eckit::DateTime creationPoint_;
    eckit::DateTime currPoint_;
    eckit::DateTime prevPoint_;
    eckit::DateTime endPoint_;
    eckit::DateTime lastFlush_;

    long count_;
    mutable std::vector<long> counts_;
    WindowType windowType_;
    // Not part of the restart state: taken from the options when a window is created or loaded
    WindowOrigin windowOrigin_;
    std::optional<eckit::DateTime> firstPoint_;
    std::map<std::int64_t, std::size_t> declaredDistanceHistogram_;
    std::map<std::int64_t, std::size_t> observedDistanceHistogram_;
    bool contiguous_;
    std::int64_t lastDeclaredDistance_;

    void initCountsLazy(size_t sz) const;

    void serialize(IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt) const;
    void deserialize(const IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt);

    void print(std::ostream& os) const;
    friend std::ostream& operator<<(std::ostream& os, const OperationWindow& a);
};

OperationWindow make_window(const std::unique_ptr<PeriodUpdater>& periodUpdater, const StatisticsConfiguration& cfg,
                            const eckit::DateTime& simulationStart);
OperationWindow load_window(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt);

}  // namespace multio::action::statistics_mtg2
