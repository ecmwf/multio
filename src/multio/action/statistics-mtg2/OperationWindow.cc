
#include "OperationWindow.h"

#include <algorithm>
#include <cinttypes>
#include <iostream>

#include "eckit/types/DateTime.h"
#include "eckit/types/Time.h"
#include "multio/LibMultio.h"
#include "multio/action/statistics-mtg2/StatisticsIO.h"

namespace multio::action::statistics_mtg2 {

namespace {

long lastDayOfTheMonth(long y, long m) {
    // month must be base 0
    long i = m - 1;
    return 31 - std::max(0L, i % 6 - i / 6) % 2
         - std::max(0L, 2 - i * (i % 2)) % 2 * (y % 4 == 0 ? y % 100 == 0 ? y % 400 == 0 ? 1 : 2 : 1 : 2);
}

void yyyymmdd2ymd(uint64_t yyyymmdd, long& y, long& m, long& d) {
    d = static_cast<long>(yyyymmdd % 100);
    m = static_cast<long>((yyyymmdd % 10000) / 100);
    y = static_cast<long>((yyyymmdd % 100000000) / 10000);
    if (m < 1 || m > 12) {
        throw eckit::SeriousBug("invalid month range", Here());
    }
    if (d < 1 || d > lastDayOfTheMonth(y, m)) {
        throw eckit::SeriousBug("invalid day range", Here());
    }
    return;
}

void hhmmss2hms(uint64_t hhmmss, long& h, long& m, long& s) {
    s = static_cast<long>(hhmmss % 100);
    m = static_cast<long>((hhmmss % 10000) / 100);
    h = static_cast<long>((hhmmss % 1000000) / 10000);
    if (s < 0 || s > 59) {
        throw eckit::SeriousBug("invalid seconds range", Here());
    }
    if (m < 0 || m > 59) {
        throw eckit::SeriousBug("invalid minutes range", Here());
    }
    if (h < 0 || h > 23) {
        throw eckit::SeriousBug("invalid hour range", Here());
    }
    return;
}

eckit::DateTime yyyymmdd_hhmmss2DateTime(uint64_t yyyymmdd, uint64_t hhmmss) {
    long dy = 0, dm = 0, dd = 0, th = 0, tm = 0, ts = 0;
    yyyymmdd2ymd(yyyymmdd, dy, dm, dd);
    hhmmss2hms(hhmmss, th, tm, ts);
    return eckit::DateTime{eckit::Date{dy, dm, dd}, eckit::Time{th, tm, ts}};
}
}  // namespace


OperationWindow make_window(const std::unique_ptr<PeriodUpdater>& periodUpdater, const StatisticsConfiguration& cfg,
                            const eckit::DateTime& simulationStart) {
    eckit::DateTime epochPoint{cfg.epoch()};
    eckit::DateTime startPoint{periodUpdater->computeWinStartTime(simulationStart)};
    eckit::DateTime endPoint{periodUpdater->computeWinEndTime(startPoint)};

    const auto isAfterWindow = [&](const eckit::DateTime& point) {
        return cfg.options().windowType() == WindowType::ForwardOffset ? point > endPoint : point >= endPoint;
    };
    while (isAfterWindow(cfg.curr())) {
        startPoint = endPoint;
        endPoint = periodUpdater->computeWinEndTime(startPoint);
    }

    eckit::DateTime creationPoint{cfg.curr()};
    return OperationWindow{epochPoint, startPoint, creationPoint, endPoint, cfg.options().windowType()};
};

OperationWindow load_window(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) {
    IOmanager->pushDir("operationWindow");
    // std::ostringstream logos;
    // logos << "     - Loading operationWindow from: " << IOmanager->getCurrentDir()  << std::endl;
    // LOG_DEBUG_LIB(LibMultio) << logos.str() << std::endl;
    OperationWindow opwin{IOmanager, opt};
    IOmanager->popDir();
    return opwin;
};


OperationWindow::OperationWindow(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) :
    epochPoint_{eckit::Date{0}, eckit::Time{0}},
    startPoint_{eckit::Date{0}, eckit::Time{0}},
    creationPoint_{eckit::Date{0}, eckit::Time{0}},
    currPoint_{eckit::Date{0}, eckit::Time{0}},
    prevPoint_{eckit::Date{0}, eckit::Time{0}},
    endPoint_{eckit::Date{0}, eckit::Time{0}},
    lastFlush_{eckit::Date{0}, eckit::Time{0}},
    count_{0},
    counts_{},
    windowType_{WindowType::ForwardOffset},
    firstPoint_{},
    declaredDistanceHistogram_{},
    observedDistanceHistogram_{},
    contiguous_{true},
    lastDeclaredDistance_{0} {
    load(IOmanager, opt);
    return;
}

OperationWindow::OperationWindow(const eckit::DateTime& epochPoint, const eckit::DateTime& startPoint,
                                 const eckit::DateTime& creationPoint, const eckit::DateTime& endPoint,
                                 WindowType windowType) :
    epochPoint_{epochPoint},
    startPoint_{startPoint},
    creationPoint_{creationPoint},
    currPoint_{creationPoint},
    prevPoint_{creationPoint},
    endPoint_{endPoint},
    lastFlush_{epochPoint},
    count_{0},
    counts_{},
    windowType_{windowType},
    firstPoint_{},
    declaredDistanceHistogram_{},
    observedDistanceHistogram_{},
    contiguous_{true},
    lastDeclaredDistance_{0} {}


long OperationWindow::count() const {
    return count_;
}

const std::vector<long>& OperationWindow::counts() const {
    return counts_;
}

template <typename T>
void OperationWindow::updateCounts(const T* values, size_t size, double missingValue) const {
    initCountsLazy(size);
    std::transform(counts_.begin(), counts_.end(), values, counts_.begin(),
                   [missingValue](long c, T v) { return v == missingValue ? c : c + 1; });
    return;
}
template void OperationWindow::updateCounts(const float* values, size_t size, double missingValue) const;
template void OperationWindow::updateCounts(const double* values, size_t size, double missingValue) const;

void OperationWindow::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const {
    const size_t writeSize = restartSize();
    IOBuffer restartState{IOmanager->getBuffer(writeSize)};
    restartState.zero();
    serialize(restartState, IOmanager->getCurrentDir() + "/operationWindow_dump.txt", opt);
    IOmanager->write("operationWindow", writeSize, writeSize);
    IOmanager->flush();
    return;
}

void OperationWindow::load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) {
    size_t readSize;
    IOmanager->readSize("operationWindow", readSize);
    IOBuffer restartState{IOmanager->getBuffer(readSize)};
    IOmanager->read("operationWindow", readSize);
    deserialize(restartState, IOmanager->getCurrentDir() + "/operationWindow_load.txt", opt);
    restartState.zero();
    return;
}

void OperationWindow::updateData(const eckit::DateTime& currentPoint, std::int64_t distanceFromPreviousStepInSeconds) {
    if (windowType_ == WindowType::ForwardOffset) {
        gtLowerBound(currentPoint, true);
        leUpperBound(currentPoint, true);
    }
    else {
        geLowerBound(currentPoint, true);
        ltUpperBound(currentPoint, true);
    }
    // Distance to the previous sample; the first sample is measured from the window start
    const auto previousPoint = firstPoint_ ? currPoint_ : startPoint_;
    const auto observedDistance = static_cast<std::int64_t>(currentPoint - previousPoint);
    declaredDistanceHistogram_[distanceFromPreviousStepInSeconds]++;
    lastDeclaredDistance_ = distanceFromPreviousStepInSeconds;

    if (!firstPoint_) {
        firstPoint_ = currentPoint;
        if (windowType_ == WindowType::ForwardOffset) {
            observedDistanceHistogram_[observedDistance]++;
            contiguous_ = observedDistance == distanceFromPreviousStepInSeconds;
        }
        else {
            contiguous_ = currentPoint == startPoint_;
        }
    }
    else {
        observedDistanceHistogram_[observedDistance]++;
        if (observedDistance != distanceFromPreviousStepInSeconds) {
            contiguous_ = false;
        }
    }

    prevPoint_ = currPoint_;
    currPoint_ = currentPoint;
    count_++;
    return;
}

void OperationWindow::updateWindow(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) {
    // TODO: probably we want to add some checks here to avoid overlapping windows?
    startPoint_ = startPoint;
    creationPoint_ = startPoint;
    currPoint_ = startPoint;
    prevPoint_ = startPoint;
    endPoint_ = endPoint;
    count_ = 0;
    counts_.clear();
    firstPoint_.reset();
    declaredDistanceHistogram_.clear();
    observedDistanceHistogram_.clear();
    contiguous_ = true;
    lastDeclaredDistance_ = 0;
    return;
}

bool OperationWindow::isWithin(const eckit::DateTime& dt) const {
    bool ret;
    if (windowType_ == WindowType::ForwardOffset) {
        ret = gtLowerBound(dt, false) && leUpperBound(dt, false);
    }
    else if (windowType_ == WindowType::BackwardOffset) {
        ret = geLowerBound(dt, false) && ltUpperBound(dt, false);
    }
    else {
        std::ostringstream os;
        os << *this << " Unknown window type " << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    LOG_DEBUG_LIB(LibMultio) << " ------ Is " << dt << " within " << *this << "? -- " << (ret ? "yes" : "no")
                             << std::endl;
    return ret;
}

bool OperationWindow::gtLowerBound(const eckit::DateTime& dt, bool throw_error) const {
    if (throw_error && startPoint_ >= dt) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : lower Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt > startPoint_;
};

bool OperationWindow::geLowerBound(const eckit::DateTime& dt, bool throw_error) const {
    if (throw_error && startPoint_ > dt) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : lower Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt >= startPoint_;
};

bool OperationWindow::leUpperBound(const eckit::DateTime& dt, bool throw_error) const {
    if (throw_error && dt > endPoint()) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : upper Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt <= endPoint();
};

bool OperationWindow::ltUpperBound(const eckit::DateTime& dt, bool throw_error) const {
    if (throw_error && dt >= endPoint()) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : upper Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt < endPoint();
};

long OperationWindow::timeSpanInHours() const {
    return long(endPoint_ - startPoint_) / 3600;
}

long OperationWindow::timeSpanInSeconds() const {
    return long(endPoint_ - startPoint_);
}

long OperationWindow::lastPointsDiffInSeconds() const {
    return long(currPoint_ - prevPoint_);
}

util::DateTimeDiff OperationWindow::lastPointsDiff() const {
    return util::dateTimeDiff(
        util::toDateInts(currPoint_.date().yyyymmdd()), util::toTimeInts(currPoint_.time().hhmmss()),
        util::toDateInts(prevPoint_.date().yyyymmdd()), util::toTimeInts(prevPoint_.time().hhmmss()));
}

long OperationWindow::startPointInSeconds() const {
    return startPoint_ - epochPoint_;
}

long OperationWindow::creationPointInSeconds() const {
    return creationPoint_ - epochPoint_;
}

long OperationWindow::endPointInSeconds() const {
    return endPoint_ - epochPoint_;
}

long OperationWindow::currPointInSeconds() const {
    return currPoint_ - epochPoint_;
}

long OperationWindow::prevPointInSeconds() const {
    return prevPoint_ - epochPoint_;
}


long OperationWindow::startPointInHours() const {
    return startPointInSeconds() / 3600;
}

long OperationWindow::creationPointInHours() const {
    return creationPointInSeconds() / 3600;
}

long OperationWindow::endPointInHours() const {
    return endPointInSeconds() / 3600;
}

long OperationWindow::currPointInHours() const {
    return currPointInSeconds() / 3600;
}

long OperationWindow::prevPointInHours() const {
    return prevPointInSeconds() / 3600;
}


long OperationWindow::startPointInSeconds(const eckit::DateTime& refPoint) const {
    return startPoint_ - refPoint;
}

long OperationWindow::creationPointInSeconds(const eckit::DateTime& refPoint) const {
    return creationPoint_ - refPoint;
}

long OperationWindow::endPointInSeconds(const eckit::DateTime& refPoint) const {
    return endPoint_ - refPoint;
}

long OperationWindow::currPointInSeconds(const eckit::DateTime& refPoint) const {
    return currPoint_ - refPoint;
}

long OperationWindow::prevPointInSeconds(const eckit::DateTime& refPoint) const {
    return prevPoint_ - refPoint;
}


long OperationWindow::startPointInHours(const eckit::DateTime& refPoint) const {
    return startPointInSeconds(refPoint) / 3600;
}

long OperationWindow::creationPointInHours(const eckit::DateTime& refPoint) const {
    return creationPointInSeconds(refPoint) / 3600;
}

long OperationWindow::endPointInHours(const eckit::DateTime& refPoint) const {
    return endPointInSeconds(refPoint) / 3600;
}

long OperationWindow::currPointInHours(const eckit::DateTime& refPoint) const {
    return currPointInSeconds(refPoint) / 3600;
}

long OperationWindow::prevPointInHours(const eckit::DateTime& refPoint) const {
    return prevPointInSeconds(refPoint) / 3600;
}


eckit::DateTime OperationWindow::epochPoint() const {
    return epochPoint_;
}

eckit::DateTime OperationWindow::startPoint() const {
    return startPoint_;
}

eckit::DateTime OperationWindow::creationPoint() const {
    return creationPoint_;
}

eckit::DateTime OperationWindow::endPoint() const {
    return endPoint_;
}

eckit::DateTime OperationWindow::currPoint() const {
    return currPoint_;
}

bool OperationWindow::isComplete() const {
    if (!firstPoint_ || !contiguous_) {
        return false;
    }
    if (windowType_ == WindowType::ForwardOffset) {
        return currPoint_ == endPoint_;
    }
    if (!isUniform()) {
        return false;
    }
    return currPoint_ + static_cast<eckit::Second>(declaredDistanceHistogram_.begin()->first) == endPoint_;
}

bool OperationWindow::isUniform() const {
    return declaredDistanceHistogram_.size() <= 1;
}

WindowType OperationWindow::windowType() const {
    return windowType_;
}

std::string OperationWindow::incompleteReason() const {
    if (!firstPoint_) {
        return "the window contains no samples";
    }
    if (!contiguous_) {
        return "observed sample spacing does not match the declared distance/extent";
    }
    if (windowType_ == WindowType::ForwardOffset) {
        std::ostringstream os;
        os << "the last sample is at " << currPoint_ << " instead of the window end " << endPoint_;
        return os.str();
    }
    if (!isUniform()) {
        return "backward-offset completeness requires a uniform declared distance/extent";
    }
    if (declaredDistanceHistogram_.empty()) {
        return "no declared sample distance/extent was recorded";
    }
    std::ostringstream os;
    os << "the last sample at " << currPoint_ << " plus the declared distance/extent "
       << declaredDistanceHistogram_.begin()->first << " seconds does not reach the window end " << endPoint_;
    return os.str();
}

std::int64_t OperationWindow::lastDeclaredDistance() const {
    return lastDeclaredDistance_;
}

const std::map<std::int64_t, std::size_t>& OperationWindow::declaredDistanceHistogram() const {
    return declaredDistanceHistogram_;
}

std::map<std::int64_t, std::size_t> OperationWindow::observedDistanceHistogram() const {
    auto histogram = observedDistanceHistogram_;
    if (firstPoint_) {
        const auto trailingDistance = static_cast<std::int64_t>(endPoint_ - currPoint_);
        if (trailingDistance > 0) {
            histogram[trailingDistance]++;
        }
    }
    return histogram;
}

eckit::DateTime OperationWindow::prevPoint() const {
    return prevPoint_;
}

std::string OperationWindow::stepRangeInHours() const {
    std::ostringstream os;
    os << std::to_string(startPointInHours()) << "-" << std::to_string(endPointInHours());
    return os.str();
}

std::string OperationWindow::stepRangeInHours(const eckit::DateTime& refPoint) const {
    std::ostringstream os;
    os << std::to_string(startPointInHours(refPoint)) << "-" << std::to_string(endPointInHours(refPoint));
    return os.str();
}

void OperationWindow::updateFlush() {
    lastFlush_ = currPoint_;
    return;
}

void OperationWindow::initCountsLazy(size_t size) const {
    if (counts_.size() == size) {
        return;
    }
    if (counts_.size() == 0) {
        counts_.resize(size, 0);
        return;
    }

    std::ostringstream os;
    os << *this << " : counts array is already initialized with a different size" << std::endl;
    throw eckit::SeriousBug(os.str(), Here());
}

void OperationWindow::serialize(IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt) const {

    if (opt.debugRestart()) {
        std::ofstream outFile(fname);
        outFile << "epochPoint_ :: " << epochPoint_ << std::endl;
        outFile << "startPoint_ :: " << startPoint_ << std::endl;
        outFile << "endPoint_ :: " << endPoint_ << std::endl;
        outFile << "creationPoint_ :: " << creationPoint_ << std::endl;
        outFile << "prevPoint_ :: " << prevPoint_ << std::endl;
        outFile << "currPoint_ :: " << currPoint_ << std::endl;
        outFile << "lastFlush_ :: " << lastFlush_ << std::endl;
        outFile << "count_ :: " << count_ << std::endl;
        outFile << "counts_.size() :: " << counts_.size() << std::endl;
        outFile << "windowType_ :: "
                << (windowType_ == WindowType::ForwardOffset ? "forward-offset" : "backward-offset") << std::endl;
        outFile << "firstPoint_ :: ";
        if (firstPoint_) {
            outFile << *firstPoint_;
        }
        else {
            outFile << "unset";
        }
        outFile << std::endl;
        outFile << "contiguous_ :: " << contiguous_ << std::endl;
        outFile << "lastDeclaredDistance_ :: " << lastDeclaredDistance_ << std::endl;
        outFile.close();
    }

    currState[0] = static_cast<std::uint64_t>(epochPoint_.date().yyyymmdd());
    currState[1] = static_cast<std::uint64_t>(epochPoint_.time().hhmmss());

    currState[2] = static_cast<std::uint64_t>(startPoint_.date().yyyymmdd());
    currState[3] = static_cast<std::uint64_t>(startPoint_.time().hhmmss());

    currState[4] = static_cast<std::uint64_t>(endPoint_.date().yyyymmdd());
    currState[5] = static_cast<std::uint64_t>(endPoint_.time().hhmmss());

    currState[6] = static_cast<std::uint64_t>(creationPoint_.date().yyyymmdd());
    currState[7] = static_cast<std::uint64_t>(creationPoint_.time().hhmmss());

    currState[8] = static_cast<std::uint64_t>(prevPoint_.date().yyyymmdd());
    currState[9] = static_cast<std::uint64_t>(prevPoint_.time().hhmmss());

    currState[10] = static_cast<std::uint64_t>(currPoint_.date().yyyymmdd());
    currState[11] = static_cast<std::uint64_t>(currPoint_.time().hhmmss());

    currState[12] = static_cast<std::uint64_t>(lastFlush_.date().yyyymmdd());
    currState[13] = static_cast<std::uint64_t>(lastFlush_.time().hhmmss());

    currState[14] = static_cast<std::uint64_t>(count_);
    currState[15] = static_cast<std::uint64_t>(windowType_);

    const size_t countsSize = counts_.size();
    currState[16] = static_cast<std::uint64_t>(countsSize);
    for (size_t i = 0; i < countsSize; ++i) {
        currState[i + 17] = static_cast<std::uint64_t>(counts_[i]);
    }

    size_t pos = 17 + countsSize;
    currState[pos++] = firstPoint_ ? 1 : 0;
    currState[pos++] = firstPoint_ ? static_cast<std::uint64_t>(firstPoint_->date().yyyymmdd()) : 0;
    currState[pos++] = firstPoint_ ? static_cast<std::uint64_t>(firstPoint_->time().hhmmss()) : 0;
    currState[pos++] = contiguous_ ? 1 : 0;
    currState[pos++] = static_cast<std::uint64_t>(lastDeclaredDistance_);
    currState[pos++] = static_cast<std::uint64_t>(declaredDistanceHistogram_.size());
    for (const auto& [distance, count] : declaredDistanceHistogram_) {
        currState[pos++] = static_cast<std::uint64_t>(distance);
        currState[pos++] = static_cast<std::uint64_t>(count);
    }
    currState[pos++] = static_cast<std::uint64_t>(observedDistanceHistogram_.size());
    for (const auto& [distance, count] : observedDistanceHistogram_) {
        currState[pos++] = static_cast<std::uint64_t>(distance);
        currState[pos++] = static_cast<std::uint64_t>(count);
    }

    currState.computeChecksum();

    return;
}

void OperationWindow::deserialize(const IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt) {

    currState.checkChecksum();
    epochPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[0]), static_cast<long>(currState[1]));
    startPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[2]), static_cast<long>(currState[3]));
    endPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[4]), static_cast<long>(currState[5]));
    creationPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[6]), static_cast<long>(currState[7]));
    prevPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[8]), static_cast<long>(currState[9]));
    currPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[10]), static_cast<long>(currState[11]));
    lastFlush_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[12]), static_cast<long>(currState[13]));
    count_ = static_cast<long>(currState[14]);
    windowType_ = static_cast<WindowType>(currState[15]);

    const auto countsSize = static_cast<size_t>(currState[16]);
    counts_.resize(countsSize);
    for (size_t i = 0; i < countsSize; ++i) {
        counts_[i] = static_cast<long>(currState[i + 17]);
    }

    size_t pos = 17 + countsSize;
    const bool hasFirstPoint = currState[pos++] != 0;
    const auto firstDate = static_cast<long>(currState[pos++]);
    const auto firstTime = static_cast<long>(currState[pos++]);
    if (hasFirstPoint) {
        firstPoint_ = yyyymmdd_hhmmss2DateTime(firstDate, firstTime);
    }
    else {
        firstPoint_.reset();
    }
    contiguous_ = currState[pos++] != 0;
    lastDeclaredDistance_ = static_cast<std::int64_t>(currState[pos++]);
    const auto declaredSize = static_cast<size_t>(currState[pos++]);
    declaredDistanceHistogram_.clear();
    for (size_t i = 0; i < declaredSize; ++i) {
        const auto distance = static_cast<std::int64_t>(currState[pos++]);
        declaredDistanceHistogram_[distance] = static_cast<std::size_t>(currState[pos++]);
    }
    const auto observedSize = static_cast<size_t>(currState[pos++]);
    observedDistanceHistogram_.clear();
    for (size_t i = 0; i < observedSize; ++i) {
        const auto distance = static_cast<std::int64_t>(currState[pos++]);
        observedDistanceHistogram_[distance] = static_cast<std::size_t>(currState[pos++]);
    }

    if (opt.debugRestart()) {
        std::ofstream outFile(fname);
        outFile << "epochPoint_ :: " << epochPoint_ << std::endl;
        outFile << "startPoint_ :: " << startPoint_ << std::endl;
        outFile << "endPoint_ :: " << endPoint_ << std::endl;
        outFile << "creationPoint_ :: " << creationPoint_ << std::endl;
        outFile << "prevPoint_ :: " << prevPoint_ << std::endl;
        outFile << "currPoint_ :: " << currPoint_ << std::endl;
        outFile << "lastFlush_ :: " << lastFlush_ << std::endl;
        outFile << "count_ :: " << count_ << std::endl;
        outFile << "counts_.size() :: " << counts_.size() << std::endl;
        outFile << "windowType_ :: "
                << (windowType_ == WindowType::ForwardOffset ? "forward-offset" : "backward-offset") << std::endl;
        outFile << "contiguous_ :: " << contiguous_ << std::endl;
        outFile << "lastDeclaredDistance_ :: " << lastDeclaredDistance_ << std::endl;
        outFile.close();
    }

    return;
}

size_t OperationWindow::restartSize() const {
    return 25 + counts_.size() + 2 * declaredDistanceHistogram_.size() + 2 * observedDistanceHistogram_.size();
}

void OperationWindow::print(std::ostream& os) const {
    os << "OperationWindow(" << startPoint_ << " to " << endPoint() << ")";
}

std::ostream& operator<<(std::ostream& os, const OperationWindow& a) {
    a.print(os);
    return os;
}

}  // namespace multio::action::statistics_mtg2
