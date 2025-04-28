
#include "OperationWindow.h"

#include <algorithm>
#include <cinttypes>
#include <iostream>

#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsIO.h"

namespace multio::action::statistics {

namespace {

std::int64_t lastDayOfTheMonth(std::int64_t y, std::int64_t m) {
    // month must be base 0
    std::int64_t i = m - 1;
    auto zero = static_cast<std::int64_t>(0L);
    return 31 - std::max(zero, i % 6 - i / 6) % 2
         - std::max(zero, 2 - i * (i % 2)) % 2 * (y % 4 == 0 ? y % 100 == 0 ? y % 400 == 0 ? 1 : 2 : 1 : 2);
}

void yyyymmdd2ymd(std::int64_t yyyymmdd, std::int64_t& y, std::int64_t& m, std::int64_t& d) {
    d = yyyymmdd % 100;
    m = (yyyymmdd % 10000) / 100;
    y = (yyyymmdd % 100000000) / 10000;
    if (m < 1 || m > 12) {
        throw eckit::SeriousBug("invalid month range", Here());
    }
    if (d < 1 || d > lastDayOfTheMonth(y, m)) {
        throw eckit::SeriousBug("invalid day range", Here());
    }
}

void hhmmss2hms(std::int64_t hhmmss, std::int64_t& h, std::int64_t& m, std::int64_t& s) {
    s = hhmmss % 100;
    m = (hhmmss % 10000) / 100;
    h = (hhmmss % 1000000) / 10000;
    if (s < 0 || s > 59) {
        throw eckit::SeriousBug("invalid seconds range", Here());
    }
    if (m < 0 || m > 59) {
        throw eckit::SeriousBug("invalid minutes range", Here());
    }
    if (h < 0 || h > 23) {
        throw eckit::SeriousBug("invalid hour range", Here());
    }
}

eckit::DateTime yyyymmdd_hhmmss2DateTime(std::int64_t yyyymmdd, std::int64_t hhmmss) {
    std::int64_t dy, dm, dd, th, tm, ts;
    yyyymmdd2ymd(yyyymmdd, dy, dm, dd);
    hhmmss2hms(hhmmss, th, tm, ts);
    return eckit::DateTime{eckit::Date{dy, dm, dd}, eckit::Time{th, tm, ts}};
}
}  // namespace


OperationWindow make_window(const std::unique_ptr<PeriodUpdater>& periodUpdater, const StatisticsConfiguration& cfg) {
    eckit::DateTime epochPoint{cfg.epoch()};
    eckit::DateTime startPoint{periodUpdater->computeWinStartTime(cfg.winStart())};
    eckit::DateTime creationPoint{periodUpdater->computeWinCreationTime(cfg.winStart())};
    eckit::DateTime endPoint{periodUpdater->computeWinEndTime(startPoint)};

    if (cfg.options().windowType() == "forward-offset") {
        return OperationWindow{epochPoint, startPoint, creationPoint, endPoint, cfg.timeStep(), FORWARD_OFFSET};
    }
    else if (cfg.options().windowType() == "backward-offset") {
        return OperationWindow{epochPoint, startPoint, creationPoint, endPoint, cfg.timeStep(), BACKWARD_OFFSET};
    }

    std::ostringstream os;
    os << " Unknown window type: " << cfg.options().windowType() << std::endl;
    throw eckit::SeriousBug(os.str(), Here());
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
    timeStepInSeconds_{0},
    count_{0},
    counts_{},
    type_{FORWARD_OFFSET} {
    load(IOmanager, opt);
}

OperationWindow::OperationWindow(const eckit::DateTime& epochPoint, const eckit::DateTime& startPoint,
                                 const eckit::DateTime& creationPoint, const eckit::DateTime& endPoint,
                                 std::int64_t timeStepInSeconds, OperationWindowType windowType) :
    epochPoint_{epochPoint},
    startPoint_{startPoint},
    creationPoint_{creationPoint},
    currPoint_{creationPoint},
    prevPoint_{creationPoint},
    endPoint_{endPoint},
    lastFlush_{epochPoint},
    timeStepInSeconds_{timeStepInSeconds},
    count_{0},
    counts_{},
    type_{windowType} {}


std::int64_t OperationWindow::count() const {
    return count_;
}

const std::vector<std::int64_t>& OperationWindow::counts() const {
    return counts_;
}

template <typename T>
void OperationWindow::updateCounts(const T* values, std::size_t size, double missingValue) const {
    initCountsLazy(size);
    std::transform(counts_.begin(), counts_.end(), values, counts_.begin(),
                   [missingValue](std::int64_t c, T v) { return v == missingValue ? c : c + 1; });
}
template void OperationWindow::updateCounts(const float* values, std::size_t size, double missingValue) const;
template void OperationWindow::updateCounts(const double* values, std::size_t size, double missingValue) const;

void OperationWindow::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const {
    const std::size_t writeSize = restartSize();
    IOBuffer restartState{IOmanager->getBuffer(writeSize)};
    restartState.zero();
    serialize(restartState, IOmanager->getCurrentDir() + "/operationWindow_dump.txt", opt);
    IOmanager->write("operationWindow", writeSize, writeSize);
    IOmanager->flush();
}

void OperationWindow::load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) {
    std::size_t readSize;
    IOmanager->readSize("operationWindow", readSize);
    IOBuffer restartState{IOmanager->getBuffer(readSize)};
    IOmanager->read("operationWindow", readSize);
    deserialize(restartState, IOmanager->getCurrentDir() + "/operationWindow_load.txt", opt);
    restartState.zero();
}

void OperationWindow::updateData(const eckit::DateTime& currentPoint) {
    gtLowerBound(currentPoint, true);
    leUpperBound(currentPoint, true);
    prevPoint_ = currPoint_;
    currPoint_ = currentPoint;
    count_++;
    LOG_DEBUG_LIB(LibMultio) << "Update window :: " << count_ << std::endl;
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
}

std::string OperationWindow::windowType() const {
    if (type_ == FORWARD_OFFSET) {
        return std::string{"forward-offset"};
    }
    else if (type_ == BACKWARD_OFFSET) {
        return std::string{"backward-offset"};
    }
    else {
        std::ostringstream os;
        os << *this << " Unknown window type " << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
}


bool OperationWindow::isWithin(const eckit::DateTime& dt) const {
    bool ret;
    if (type_ == FORWARD_OFFSET) {
        ret = gtLowerBound(dt, false) && leUpperBound(dt, false);
    }
    else if (type_ == BACKWARD_OFFSET) {
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
    if (throw_error && creationPoint_ >= dt) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : lower Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt > creationPoint_;
};

bool OperationWindow::geLowerBound(const eckit::DateTime& dt, bool throw_error) const {
    if (throw_error && creationPoint_ > dt) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : lower Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt >= creationPoint_;
};

bool OperationWindow::leUpperBound(const eckit::DateTime& dt, bool throw_error) const {
    // TODO: test without 1 second added. Now it should work
    if (throw_error && dt > endPoint()) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : upper Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt <= endPoint();
};

bool OperationWindow::ltUpperBound(const eckit::DateTime& dt, bool throw_error) const {
    // TODO: test without 1 second added. Now it should work
    if (throw_error && dt >= endPoint()) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : upper Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt < endPoint();
};

std::int64_t OperationWindow::timeSpanInHours() const {
    return std::int64_t(endPoint_ - creationPoint_) / 3600;
}

std::int64_t OperationWindow::timeSpanInSeconds() const {
    return std::int64_t(endPoint_ - creationPoint_);
}

std::int64_t OperationWindow::timeSpanInSteps() const {
    return timeSpanInSeconds() / timeStepInSeconds_;
}

std::int64_t OperationWindow::lastPointsDiffInSeconds() const {
    return std::int64_t(currPoint_ - prevPoint_);
}

util::DateTimeDiff OperationWindow::lastPointsDiff() const {
    return util::dateTimeDiff(
        util::toDateInts(currPoint_.date().yyyymmdd()), util::toTimeInts(currPoint_.time().hhmmss()),
        util::toDateInts(prevPoint_.date().yyyymmdd()), util::toTimeInts(prevPoint_.time().hhmmss()));
}

std::int64_t OperationWindow::startPointInSeconds() const {
    return startPoint_ - epochPoint_;
}

std::int64_t OperationWindow::creationPointInSeconds() const {
    return creationPoint_ - epochPoint_;
}

std::int64_t OperationWindow::endPointInSeconds() const {
    return endPoint_ - epochPoint_;
}

std::int64_t OperationWindow::currPointInSeconds() const {
    return currPoint_ - epochPoint_;
}

std::int64_t OperationWindow::prevPointInSeconds() const {
    return prevPoint_ - epochPoint_;
}


std::int64_t OperationWindow::startPointInHours() const {
    return startPointInSeconds() / 3600;
}

std::int64_t OperationWindow::creationPointInHours() const {
    return creationPointInSeconds() / 3600;
}

std::int64_t OperationWindow::endPointInHours() const {
    return endPointInSeconds() / 3600;
}

std::int64_t OperationWindow::currPointInHours() const {
    return currPointInSeconds() / 3600;
}

std::int64_t OperationWindow::prevPointInHours() const {
    return prevPointInSeconds() / 3600;
}


std::int64_t OperationWindow::startPointInSteps() const {
    return startPointInSeconds() / timeStepInSeconds_;
}

std::int64_t OperationWindow::creationPointInSteps() const {
    return creationPointInSeconds() / timeStepInSeconds_;
}

std::int64_t OperationWindow::endPointInSteps() const {
    return endPointInSeconds() / timeStepInSeconds_;
}

std::int64_t OperationWindow::currPointInSteps() const {
    return currPointInSeconds() / timeStepInSeconds_;
}

std::int64_t OperationWindow::prevPointInSteps() const {
    return prevPointInSeconds() / timeStepInSeconds_;
}

std::int64_t OperationWindow::startPointInSeconds(const eckit::DateTime& refPoint) const {
    return startPoint_ - refPoint;
}

std::int64_t OperationWindow::creationPointInSeconds(const eckit::DateTime& refPoint) const {
    return creationPoint_ - refPoint;
}

std::int64_t OperationWindow::endPointInSeconds(const eckit::DateTime& refPoint) const {
    return endPoint_ - refPoint;
}

std::int64_t OperationWindow::currPointInSeconds(const eckit::DateTime& refPoint) const {
    return currPoint_ - refPoint;
}

std::int64_t OperationWindow::prevPointInSeconds(const eckit::DateTime& refPoint) const {
    return prevPoint_ - refPoint;
}


std::int64_t OperationWindow::startPointInHours(const eckit::DateTime& refPoint) const {
    return startPointInSeconds(refPoint) / 3600;
}

std::int64_t OperationWindow::creationPointInHours(const eckit::DateTime& refPoint) const {
    return creationPointInSeconds(refPoint) / 3600;
}

std::int64_t OperationWindow::endPointInHours(const eckit::DateTime& refPoint) const {
    return endPointInSeconds(refPoint) / 3600;
}

std::int64_t OperationWindow::currPointInHours(const eckit::DateTime& refPoint) const {
    return currPointInSeconds(refPoint) / 3600;
}

std::int64_t OperationWindow::prevPointInHours(const eckit::DateTime& refPoint) const {
    return prevPointInSeconds(refPoint) / 3600;
}


std::int64_t OperationWindow::startPointInSteps(const eckit::DateTime& refPoint) const {
    return startPointInSeconds(refPoint) / timeStepInSeconds_;
}

std::int64_t OperationWindow::creationPointInSteps(const eckit::DateTime& refPoint) const {
    return creationPointInSeconds(refPoint) / timeStepInSeconds_;
}

std::int64_t OperationWindow::endPointInSteps(const eckit::DateTime& refPoint) const {
    return endPointInSeconds(refPoint) / timeStepInSeconds_;
}

std::int64_t OperationWindow::currPointInSteps(const eckit::DateTime& refPoint) const {
    return currPointInSeconds(refPoint) / timeStepInSeconds_;
}

std::int64_t OperationWindow::prevPointInSteps(const eckit::DateTime& refPoint) const {
    return prevPointInSeconds(refPoint) / timeStepInSeconds_;
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

eckit::DateTime OperationWindow::prevPoint() const {
    return prevPoint_;
}

std::string OperationWindow::stepRange() const {
    std::ostringstream os;
    os << std::to_string(creationPointInSteps()) << "-" << std::to_string(endPointInSteps());
    return os.str();
};

std::string OperationWindow::stepRangeInHours() const {
    std::ostringstream os;
    os << std::to_string(creationPointInHours()) << "-" << std::to_string(endPointInHours());
    return os.str();
}

std::string OperationWindow::stepRange(const eckit::DateTime& refPoint) const {
    std::ostringstream os;
    os << std::to_string(creationPointInSteps(refPoint)) << "-" << std::to_string(endPointInSteps(refPoint));
    return os.str();
};

std::string OperationWindow::stepRangeInHours(const eckit::DateTime& refPoint) const {
    std::ostringstream os;
    os << std::to_string(creationPointInHours(refPoint)) << "-" << std::to_string(endPointInHours(refPoint));
    return os.str();
}

void OperationWindow::updateFlush() {
    lastFlush_ = currPoint_;
}

std::int64_t OperationWindow::lastFlushInSteps() const {
    return (lastFlush_ - epochPoint_) / timeStepInSeconds_;
}

void OperationWindow::initCountsLazy(std::size_t size) const {
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
        outFile << "timeStepInSeconds_ :: " << timeStepInSeconds_ << std::endl;
        outFile << "count_ :: " << count_ << std::endl;
        outFile << "counts_.size() :: " << counts_.size() << std::endl;
        outFile << "type_ :: " << type_ << std::endl;
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

    currState[14] = static_cast<std::uint64_t>(timeStepInSeconds_);
    currState[15] = static_cast<std::uint64_t>(count_);
    currState[16] = static_cast<std::uint64_t>(type_);

    const std::size_t countsSize = counts_.size();
    currState[17] = static_cast<std::uint64_t>(countsSize);
    for (std::size_t i = 0; i < countsSize; ++i) {
        currState[i+18] = static_cast<std::uint64_t>(counts_[i]);
    }

    currState.computeChecksum();
}

void OperationWindow::deserialize(const IOBuffer& currState, const std::string& fname, const StatisticsOptions& opt) {

    currState.checkChecksum();
    epochPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<std::int64_t>(currState[0]), static_cast<std::int64_t>(currState[1]));
    startPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<std::int64_t>(currState[2]), static_cast<std::int64_t>(currState[3]));
    endPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<std::int64_t>(currState[4]), static_cast<std::int64_t>(currState[5]));
    creationPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<std::int64_t>(currState[6]), static_cast<std::int64_t>(currState[7]));
    prevPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<std::int64_t>(currState[8]), static_cast<std::int64_t>(currState[9]));
    currPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<std::int64_t>(currState[10]), static_cast<std::int64_t>(currState[11]));
    lastFlush_ = yyyymmdd_hhmmss2DateTime(static_cast<std::int64_t>(currState[12]), static_cast<std::int64_t>(currState[13]));
    timeStepInSeconds_ = static_cast<std::int64_t>(currState[14]);
    count_ = static_cast<std::int64_t>(currState[15]);
    type_ = static_cast<OperationWindowType>(currState[16]);

    const auto countsSize = static_cast<std::size_t>(currState[17]);
    counts_.resize(countsSize);
    for (std::size_t i = 0; i < countsSize; ++i) {
        counts_[i] = static_cast<std::int64_t>(currState[i+18]);
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
        outFile << "timeStepInSeconds_ :: " << timeStepInSeconds_ << std::endl;
        outFile << "count_ :: " << count_ << std::endl;
        outFile << "counts_.size() :: " << counts_.size() << std::endl;
        outFile << "type_ :: " << type_ << std::endl;
        outFile.close();
    }
}

std::size_t OperationWindow::restartSize() const {
    return 18 + counts_.size() + 1;  // values + counts + checksum
}

void OperationWindow::print(std::ostream& os) const {
    os << "OperationWindow(" << startPoint_ << " to " << endPoint() << ")";
}

std::ostream& operator<<(std::ostream& os, const OperationWindow& a) {
    a.print(os);
    return os;
}

}  // namespace multio::action::statistics
