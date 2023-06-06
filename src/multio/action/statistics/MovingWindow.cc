
#include "MovingWindow.h"

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
    h = static_cast<long>(hhmmss % 100);
    m = static_cast<long>((hhmmss % 10000) / 100);
    s = static_cast<long>((hhmmss % 1000000) / 10000);
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


MovingWindow::MovingWindow(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) :
    epochPoint_{eckit::Date{0}, eckit::Time{0}},
    startPoint_{eckit::Date{0}, eckit::Time{0}},
    creationPoint_{eckit::Date{0}, eckit::Time{0}},
    currPoint_{eckit::Date{0}, eckit::Time{0}},
    prevPoint_{eckit::Date{0}, eckit::Time{0}},
    endPoint_{eckit::Date{0}, eckit::Time{0}},
    timeStepInSeconds_{0},
    count_{0} {
    load(IOmanager, cfg);
    return;
}

MovingWindow::MovingWindow(const eckit::DateTime& epochPoint, const eckit::DateTime& startPoint,
                           const eckit::DateTime& creationPoint, const eckit::DateTime& endPoint,
                           long timeStepInSeconds) :
    epochPoint_{epochPoint},
    startPoint_{startPoint},
    creationPoint_{creationPoint},
    currPoint_{creationPoint},
    prevPoint_{creationPoint},
    endPoint_{endPoint},
    timeStepInSeconds_{timeStepInSeconds},
    count_{0} {}

long MovingWindow::count() const {
    return count_;
}

void MovingWindow::load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) {
    std::vector<std::uint64_t> restartState(15, 0);
    IOmanager->read("window", restartState);
    deserialize(restartState);
    return;
}

void MovingWindow::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
    std::vector<std::uint64_t> restartState(15, 0);
    serialize(restartState);
    IOmanager->write("window", restartState);
    IOmanager->flush();
    return;
}

void MovingWindow::updateData(const eckit::DateTime& currentPoint) {
    gtLowerBound(currentPoint, true);
    leUpperBound(currentPoint, true);
    prevPoint_ = currPoint_;
    currPoint_ = currentPoint;
    count_++;
    std::cout << "Update window :: " << count_ << std::endl;
    return;
}

void MovingWindow::updateWindow(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) {
    // TODO: probably we want to add some checks here to avoid overlapping windows?
    startPoint_ = startPoint;
    creationPoint_ = startPoint;
    currPoint_ = startPoint;
    prevPoint_ = startPoint;
    endPoint_ = endPoint;
    count_ = 0;
    return;
}


bool MovingWindow::isWithin(const eckit::DateTime& dt) const {
    bool ret = gtLowerBound(dt, true) && leUpperBound(dt, false);
    LOG_DEBUG_LIB(LibMultio) << " ------ Is " << dt << " within " << *this << "? -- " << (ret ? "yes" : "no")
                             << std::endl;
    return ret;
}

bool MovingWindow::gtLowerBound(const eckit::DateTime& dt, bool throw_error) const {
    if (throw_error && creationPoint_ >= dt) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : lower Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt > creationPoint_;
};

bool MovingWindow::leUpperBound(const eckit::DateTime& dt, bool throw_error) const {
    // TODO: test without 1 second added. Now it should work
    if (throw_error && dt > endPoint() + eckit::Second{1.0}) {
        std::ostringstream os;
        os << *this << " : " << dt << " is outside of current period : upper Bound violation" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return dt <= endPoint() + eckit::Second{1.0};
};

long MovingWindow::timeSpanInHours() const {
    return long(endPoint_ - creationPoint_) / 3600;
}

long MovingWindow::timeSpanInSeconds() const {
    return long(endPoint_ - creationPoint_);
}

long MovingWindow::timeSpanInSteps() const {
    return timeSpanInSeconds() / timeStepInSeconds_;
}

long MovingWindow::lastPointsDiffInSeconds() const {
    return long(currPoint_ - prevPoint_);
}


long MovingWindow::startPointInSeconds() const {
    return startPoint_ - epochPoint_;
}

long MovingWindow::creationPointInSeconds() const {
    return creationPoint_ - epochPoint_;
}

long MovingWindow::endPointInSeconds() const {
    return endPoint_ - epochPoint_;
}

long MovingWindow::currPointInSeconds() const {
    return currPoint_ - epochPoint_;
}

long MovingWindow::prevPointInSeconds() const {
    return prevPoint_ - epochPoint_;
}


long MovingWindow::startPointInHours() const {
    return startPointInSeconds() / 3600;
}

long MovingWindow::creationPointInHours() const {
    return creationPointInSeconds() / 3600;
}

long MovingWindow::endPointInHours() const {
    return endPointInSeconds() / 3600;
}

long MovingWindow::currPointInHours() const {
    return currPointInSeconds() / 3600;
}

long MovingWindow::prevPointInHours() const {
    return prevPointInSeconds() / 3600;
}


long MovingWindow::startPointInSteps() const {
    return startPointInSeconds() / timeStepInSeconds_;
}

long MovingWindow::creationPointInSteps() const {
    return creationPointInSeconds() / timeStepInSeconds_;
}

long MovingWindow::endPointInSteps() const {
    return endPointInSeconds() / timeStepInSeconds_;
}

long MovingWindow::currPointInSteps() const {
    return currPointInSeconds() / timeStepInSeconds_;
}

long MovingWindow::prevPointInSteps() const {
    return prevPointInSeconds() / timeStepInSeconds_;
}

long MovingWindow::startPointInSeconds(const eckit::DateTime& refPoint) const {
    return startPoint_ - refPoint;
}

long MovingWindow::creationPointInSeconds(const eckit::DateTime& refPoint) const {
    return creationPoint_ - refPoint;
}

long MovingWindow::endPointInSeconds(const eckit::DateTime& refPoint) const {
    return endPoint_ - refPoint;
}

long MovingWindow::currPointInSeconds(const eckit::DateTime& refPoint) const {
    return currPoint_ - refPoint;
}

long MovingWindow::prevPointInSeconds(const eckit::DateTime& refPoint) const {
    return prevPoint_ - refPoint;
}


long MovingWindow::startPointInHours(const eckit::DateTime& refPoint) const {
    return startPointInSeconds(refPoint) / 3600;
}

long MovingWindow::creationPointInHours(const eckit::DateTime& refPoint) const {
    return creationPointInSeconds(refPoint) / 3600;
}

long MovingWindow::endPointInHours(const eckit::DateTime& refPoint) const {
    return endPointInSeconds(refPoint) / 3600;
}

long MovingWindow::currPointInHours(const eckit::DateTime& refPoint) const {
    return currPointInSeconds(refPoint) / 3600;
}

long MovingWindow::prevPointInHours(const eckit::DateTime& refPoint) const {
    return prevPointInSeconds(refPoint) / 3600;
}


long MovingWindow::startPointInSteps(const eckit::DateTime& refPoint) const {
    return startPointInSeconds(refPoint) / timeStepInSeconds_;
}

long MovingWindow::creationPointInSteps(const eckit::DateTime& refPoint) const {
    return creationPointInSeconds(refPoint) / timeStepInSeconds_;
}

long MovingWindow::endPointInSteps(const eckit::DateTime& refPoint) const {
    return endPointInSeconds(refPoint) / timeStepInSeconds_;
}

long MovingWindow::currPointInSteps(const eckit::DateTime& refPoint) const {
    return currPointInSeconds(refPoint) / timeStepInSeconds_;
}

long MovingWindow::prevPointInSteps(const eckit::DateTime& refPoint) const {
    return prevPointInSeconds(refPoint) / timeStepInSeconds_;
}

eckit::DateTime MovingWindow::epochPoint() const {
    return epochPoint_;
}

eckit::DateTime MovingWindow::startPoint() const {
    return startPoint_;
}

eckit::DateTime MovingWindow::creationPoint() const {
    return creationPoint_;
}

eckit::DateTime MovingWindow::endPoint() const {
    return endPoint_;
}

eckit::DateTime MovingWindow::currPoint() const {
    return currPoint_;
}

eckit::DateTime MovingWindow::prevPoint() const {
    return prevPoint_;
}

const std::string MovingWindow::stepRange() const {
    std::ostringstream os;
    os << std::to_string(creationPointInSteps()) << "-" << std::to_string(endPointInSteps());
    return os.str();
};

const std::string MovingWindow::stepRangeInHours() const {
    std::ostringstream os;
    os << std::to_string(creationPointInHours()) << "-" << std::to_string(endPointInHours());
    return os.str();
}

const std::string MovingWindow::stepRange(const eckit::DateTime& refPoint) const {
    std::ostringstream os;
    os << std::to_string(creationPointInSteps(refPoint)) << "-" << std::to_string(endPointInSteps(refPoint));
    return os.str();
};

const std::string MovingWindow::stepRangeInHours(const eckit::DateTime& refPoint) const {
    std::ostringstream os;
    os << std::to_string(creationPointInHours(refPoint)) << "-" << std::to_string(endPointInHours(refPoint));
    return os.str();
}

void MovingWindow::serialize(std::vector<std::uint64_t>& currState) const {


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

    currState[12] = static_cast<std::uint64_t>(timeStepInSeconds_);
    currState[13] = static_cast<std::uint64_t>(count_);

    currState[14] = computeChecksum(currState);

    return;
}

void MovingWindow::deserialize(const std::vector<std::uint64_t>& currState) {

    if (currState[14] != computeChecksum(currState)) {
        throw eckit::SeriousBug("Checksum mismatch!", Here());
    }
    epochPoint_ = yyyymmdd_hhmmss2DateTime(currState[0], currState[1]);
    startPoint_ = yyyymmdd_hhmmss2DateTime(currState[2], currState[3]);
    endPoint_ = yyyymmdd_hhmmss2DateTime(currState[4], currState[5]);
    creationPoint_ = yyyymmdd_hhmmss2DateTime(currState[6], currState[7]);
    prevPoint_ = yyyymmdd_hhmmss2DateTime(currState[8], currState[9]);
    currPoint_ = yyyymmdd_hhmmss2DateTime(currState[10], currState[11]);
    timeStepInSeconds_ = static_cast<long>(currState[12]);
    count_ = static_cast<long>(currState[13]);

    return;
}

void MovingWindow::print(std::ostream& os) const {
    os << "MovingWindow(" << startPoint_ << " to " << endPoint() << ")";
}

std::ostream& operator<<(std::ostream& os, const MovingWindow& a) {
    a.print(os);
    return os;
}

}  // namespace multio::action
