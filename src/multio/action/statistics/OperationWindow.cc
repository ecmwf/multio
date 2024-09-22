
#include "OperationWindow.h"

#include <algorithm>
#include <cinttypes>
#include <iostream>

#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsIO.h"

namespace multio::action {

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


OperationWindow make_window( const std::unique_ptr<PeriodUpdater>& periodUpdater, const StatisticsConfiguration& cfg) {
    eckit::DateTime epochPoint{cfg.epoch()};
    eckit::DateTime startPoint{periodUpdater->computeWinStartTime(cfg.winStart())};
    eckit::DateTime creationPoint{periodUpdater->computeWinCreationTime(cfg.winStart())};
    eckit::DateTime endPoint{periodUpdater->computeWinEndTime(startPoint)};
    return OperationWindow{epochPoint, startPoint, creationPoint, endPoint, cfg.timeStep()};
};

OperationWindow load_window( std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt ) {
    IOmanager->pushDir( "operationWindow" );
    std::ostringstream logos;
    logos << " - Loading operationWindow from: " << IOmanager->getCurrentDir()  << std::endl;
    std::cout << logos.str() << std::endl;
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
    count_{0} {
    load(IOmanager, opt);
    return;
}

OperationWindow::OperationWindow(const eckit::DateTime& epochPoint, const eckit::DateTime& startPoint,
                                 const eckit::DateTime& creationPoint, const eckit::DateTime& endPoint,
                                 long timeStepInSeconds) :
    epochPoint_{epochPoint},
    startPoint_{startPoint},
    creationPoint_{creationPoint},
    currPoint_{creationPoint},
    prevPoint_{creationPoint},
    endPoint_{endPoint},
    lastFlush_{epochPoint},
    timeStepInSeconds_{timeStepInSeconds},
    count_{0} {}


long OperationWindow::count() const {
    return count_;
}

void OperationWindow::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const {
    IOBuffer restartState{IOmanager->getBuffer(restartSize())};
    restartState.zero();
    serialize(restartState);
    IOmanager->write("operationWindow", static_cast<size_t>(16), restartSize() );
    IOmanager->flush();
    return;
}

void OperationWindow::load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) {
    IOBuffer restartState{IOmanager->getBuffer(restartSize())};
    IOmanager->read( "operationWindow", restartSize() );
    deserialize(restartState);
    restartState.zero();
    return;
}

void OperationWindow::updateData(const eckit::DateTime& currentPoint) {
    gtLowerBound(currentPoint, true);
    leUpperBound(currentPoint, true);
    prevPoint_ = currPoint_;
    currPoint_ = currentPoint;
    count_++;
    LOG_DEBUG_LIB(LibMultio) << "Update window :: " << count_ << std::endl;
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
    return;
}


bool OperationWindow::isWithin(const eckit::DateTime& dt) const {
    bool ret = gtLowerBound(dt, true) && leUpperBound(dt, false);
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

bool OperationWindow::leUpperBound(const eckit::DateTime& dt, bool throw_error) const {
    // TODO: test without 1 second added. Now it should work
    if (throw_error && dt > endPoint()) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : upper Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt <= endPoint();
};

long OperationWindow::timeSpanInHours() const {
    return long(endPoint_ - creationPoint_) / 3600;
}

long OperationWindow::timeSpanInSeconds() const {
    return long(endPoint_ - creationPoint_);
}

long OperationWindow::timeSpanInSteps() const {
    return timeSpanInSeconds() / timeStepInSeconds_;
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


long OperationWindow::startPointInSteps() const {
    return startPointInSeconds() / timeStepInSeconds_;
}

long OperationWindow::creationPointInSteps() const {
    return creationPointInSeconds() / timeStepInSeconds_;
}

long OperationWindow::endPointInSteps() const {
    return endPointInSeconds() / timeStepInSeconds_;
}

long OperationWindow::currPointInSteps() const {
    return currPointInSeconds() / timeStepInSeconds_;
}

long OperationWindow::prevPointInSteps() const {
    return prevPointInSeconds() / timeStepInSeconds_;
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


long OperationWindow::startPointInSteps(const eckit::DateTime& refPoint) const {
    return startPointInSeconds(refPoint) / timeStepInSeconds_;
}

long OperationWindow::creationPointInSteps(const eckit::DateTime& refPoint) const {
    return creationPointInSeconds(refPoint) / timeStepInSeconds_;
}

long OperationWindow::endPointInSteps(const eckit::DateTime& refPoint) const {
    return endPointInSeconds(refPoint) / timeStepInSeconds_;
}

long OperationWindow::currPointInSteps(const eckit::DateTime& refPoint) const {
    return currPointInSeconds(refPoint) / timeStepInSeconds_;
}

long OperationWindow::prevPointInSteps(const eckit::DateTime& refPoint) const {
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
    return;
}

long OperationWindow::lastFlushInSteps() const {
    return (lastFlush_ - epochPoint_) / timeStepInSeconds_;
}

void OperationWindow::serialize(IOBuffer& currState) const {


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

    currState[12] = static_cast<std::uint64_t>(currPoint_.date().yyyymmdd());
    currState[13] = static_cast<std::uint64_t>(currPoint_.time().hhmmss());

    currState[14] = static_cast<std::uint64_t>(timeStepInSeconds_);
    currState[15] = static_cast<std::uint64_t>(count_);

    currState.computeChecksum();

    return;
}

void OperationWindow::deserialize(const IOBuffer& currState) {

    currState.checkChecksum();
    epochPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[0]), static_cast<long>(currState[1]));
    startPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[2]), static_cast<long>(currState[3]));
    endPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[4]), static_cast<long>(currState[5]));
    creationPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[6]), static_cast<long>(currState[7]));
    prevPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[8]), static_cast<long>(currState[9]));
    currPoint_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[10]), static_cast<long>(currState[11]));
    lastFlush_ = yyyymmdd_hhmmss2DateTime(static_cast<long>(currState[12]), static_cast<long>(currState[13]));
    timeStepInSeconds_ = static_cast<long>(currState[14]);
    count_ = static_cast<long>(currState[15]);

    return;
}

size_t OperationWindow::restartSize() const {
    return static_cast<size_t>(17);
}

void OperationWindow::print(std::ostream& os) const {
    os << "OperationWindow(" << startPoint_ << " to " << endPoint() << ")";
}

std::ostream& operator<<(std::ostream& os, const OperationWindow& a) {
    a.print(os);
    return os;
}

}  // namespace multio::action
