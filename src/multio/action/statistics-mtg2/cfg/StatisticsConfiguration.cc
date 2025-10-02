#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"

#include <limits.h>
#include <unistd.h>
#include <cstdint>
#include <iomanip>
#include <optional>
#include <string>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "multio/LibMultio.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/Glossary.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/util/Substitution.h"

namespace multio::action::statistics_mtg2 {

namespace dm = multio::datamod;


std::int64_t readDate(const message::Metadata& md) {
    // NOTE: Currently no support for messages without step (from analysis)!
    ASSERT(md.getOpt<std::int64_t>(dm::legacy::Step).has_value());
    const auto dateVal = md.getOpt<std::int64_t>(dm::legacy::Date);
    ASSERT(dateVal.has_value());
    return *dateVal;
}

std::int64_t readTime(const message::Metadata& md) {
    // NOTE: Currently no support for messages without step (from analysis)!
    ASSERT(md.getOpt<std::int64_t>(dm::legacy::Step).has_value());
    const auto timeVal = md.getOpt<std::int64_t>(dm::legacy::Time);
    ASSERT(timeVal.has_value());
    return *timeVal;
}

std::int64_t readLevel(const message::Metadata& md) {
    if (auto level = md.getOpt<std::int64_t>(dm::legacy::Level); level) {
        return *level;
    }
    else if (auto levelist = md.getOpt<std::int64_t>(dm::legacy::Levelist); levelist) {
        return *levelist;
    }
    // if none of the above metadata options are present the default levtype is 0
    return 0;
}

std::int64_t readTimeStep(const message::Metadata& md, int64_t defaultTimeStep) {
    return md.getOpt<std::int64_t>(dm::legacy::TimeStep).value_or(defaultTimeStep);
}

std::int64_t readStep(const message::Metadata& md) {
    // NOTE: Currently no support for messages without step (from analysis)!
    const auto stepVal = md.getOpt<std::int64_t>(dm::legacy::Step);
    ASSERT(stepVal.has_value());
    return *stepVal;
}

std::string readParam(const message::Metadata& md) {
    // TODO: use whole validated keyset...
    if (const auto& grid = dm::parseEntry(dm::PARAM, md); grid.isSet()) {
        return std::to_string(grid.get());
    }
    else if (auto paramId = md.getOpt<std::int64_t>(dm::legacy::ParamId); paramId) {
        return std::to_string(*paramId);
    }
    throw eckit::SeriousBug{"Param metadata not present", Here()};
}

std::string readLevType(const message::Metadata& md) {
    if (auto levType = md.getOpt<std::string>(dm::legacy::Levtype); levType) {
        return *levType;
    }
    throw message::MetadataException("Levtype missing in metadata", Here());
}

std::string readGridType(const message::Metadata& md) {
    // TODO use whole validated keyset...
    if (const auto& grid = dm::parseEntry(dm::GRID, md); grid.isSet()) {
        return grid.get();
    }

    // Truncation is always present when we are dealing with Spherical Harmonics
    if (dm::parseEntry(dm::TRUNCATION, md).isSet()) {
        return "none";
    }

    std::ostringstream os;
    os << "Cannot find grid or truncation in metadata : " << md;
    throw eckit::SeriousBug{os.str(), Here()};
}

std::string readPrecision(const message::Metadata& md) {
    if (auto precision = md.getOpt<std::string>(dm::legacy::Precision); precision) {
        return *precision;
    }
    throw eckit::SeriousBug{"precision metadata not present", Here()};
}

std::optional<double> readMissingValue(const message::Metadata& md) {
    const auto missingVal = md.getOpt<double>(dm::legacy::MissingValue);
    const auto bitMapPresent = md.getOpt<bool>(dm::legacy::BitmapPresent);

    if (missingVal && bitMapPresent && *bitMapPresent) {
        return missingVal;
    }
    return std::nullopt;
}


StatisticsConfiguration::StatisticsConfiguration(const message::Metadata& md, const message::Peer& src,
                                                 const StatisticsOptions& opt) :
    opt_{opt},
    date_{readDate(md)},
    time_{readTime(md)},
    level_{readLevel(md)},
    timeStep_{readTimeStep(md, opt.timeStep())},
    step_{readStep(md)},
    param_{readParam(md)},
    levType_{readLevType(md)},
    gridType_{readGridType(md)},
    precision_{readPrecision(md)},
    missingValue_{readMissingValue(md)},
    key_{generateKey(src)} {
    // Associate local procedure pointers
    computeEpoch_ = std::bind(&StatisticsConfiguration::computeEpoch, this);
    computeCurr_ = std::bind(&StatisticsConfiguration::computeCurr, this);
    computeWinStart_ = std::bind(&StatisticsConfiguration::computeWinStart, this);
    computeBeginningOfHour_ = std::bind(&StatisticsConfiguration::computeBeginningOfHour, this);
    computeBeginningOfDay_ = std::bind(&StatisticsConfiguration::computeBeginningOfDay, this);
    computeBeginningOfMonth_ = std::bind(&StatisticsConfiguration::computeBeginningOfMonth, this);
    computeBeginningOfYear_ = std::bind(&StatisticsConfiguration::computeBeginningOfYear, this);
}

StatisticsConfiguration::StatisticsConfiguration(const message::Message& msg, const StatisticsOptions& opt) :
    StatisticsConfiguration(msg.metadata(), msg.source(), opt) {};


std::string StatisticsConfiguration::generateKey(const message::Peer& src) const {
    std::ostringstream os;
    os << param_ << "-" << level_ << "-" << levType_ << "-" << gridType_ << "-" << precision_ << "-" << src.group()
       << "_" << src.id();
    return os.str();
}


eckit::DateTime StatisticsConfiguration::epoch() const {
    epoch_ = computeEpoch_();
    return epoch_;
}

eckit::DateTime StatisticsConfiguration::curr() const {
    curr_ = computeCurr_();
    return curr_;
}

eckit::DateTime StatisticsConfiguration::winStart() const {
    winStart_ = computeWinStart_();
    return winStart_;
}

bool StatisticsConfiguration::beginningOfHour() const {
    auto ret = computeBeginningOfHour_();
    return ret;
}

bool StatisticsConfiguration::beginningOfDay() const {
    auto ret = computeBeginningOfDay_();
    return ret;
}

bool StatisticsConfiguration::beginningOfMonth() const {
    auto ret = computeBeginningOfMonth_();
    return ret;
}

bool StatisticsConfiguration::beginningOfYear() const {
    auto ret = computeBeginningOfYear_();
    return ret;
}


eckit::DateTime StatisticsConfiguration::computeEpoch() const {
    eckit::Date startDate{date_};
    auto hour = time_ / 10000;
    auto minute = (time_ % 10000) / 100;
    epoch_ = eckit::DateTime{startDate, eckit::Time{hour, minute, 0}};
    computeEpoch_ = std::bind(&StatisticsConfiguration::getEpoch, this);
    return epoch_;
};

eckit::DateTime StatisticsConfiguration::getEpoch() const {
    return epoch_;
}


eckit::DateTime StatisticsConfiguration::computeCurr() const {
    curr_ = epoch() + static_cast<eckit::Second>(std::max((step_), 0L) * timeStep_);
    computeCurr_ = std::bind(&StatisticsConfiguration::getCurr, this);
    return curr_;
}

eckit::DateTime StatisticsConfiguration::getCurr() const {
    return curr_;
}


eckit::DateTime StatisticsConfiguration::computeWinStart() const {
    ASSERT(opt_.solver_send_initial_condition());
    winStart_ = curr();
    computeWinStart_ = std::bind(&StatisticsConfiguration::getWinStart, this);
    return winStart_;
}

eckit::DateTime StatisticsConfiguration::getWinStart() const {
    return winStart_;
}


bool StatisticsConfiguration::computeBeginningOfHour() const {
    eckit::DateTime now = curr();
    long min = now.time().minutes();
    long sec = now.time().seconds();
    beginningOfHour_ = (min == 0 && sec == 0);
    computeBeginningOfHour_ = std::bind(&StatisticsConfiguration::isBeginningOfHour, this);
    return beginningOfHour_;
}

bool StatisticsConfiguration::isBeginningOfHour() const {
    return beginningOfHour_;
}

bool StatisticsConfiguration::computeBeginningOfDay() const {
    eckit::DateTime now = curr();
    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();
    beginningOfDay_ = (hour == 0 && min == 0 && sec == 0);
    computeBeginningOfDay_ = std::bind(&StatisticsConfiguration::isBeginningOfDay, this);
    return beginningOfDay_;
}

bool StatisticsConfiguration::isBeginningOfDay() const {
    return beginningOfDay_;
}

bool StatisticsConfiguration::computeBeginningOfMonth() const {
    eckit::DateTime now = curr();
    long day = now.date().day();
    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();
    beginningOfMonth_ = (day == 1 && hour == 0 && min == 0 && sec == 0);
    computeBeginningOfMonth_ = std::bind(&StatisticsConfiguration::isBeginningOfMonth, this);
    return beginningOfMonth_;
}

bool StatisticsConfiguration::isBeginningOfMonth() const {
    return beginningOfMonth_;
}

bool StatisticsConfiguration::computeBeginningOfYear() const {
    eckit::DateTime now = curr();
    long month = now.date().month();
    long day = now.date().day();
    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();
    beginningOfYear_ = (month == 1 && day == 1 && hour == 0 && min == 0 && sec == 0);
    computeBeginningOfYear_ = std::bind(&StatisticsConfiguration::isBeginningOfYear, this);
    return beginningOfYear_;
}

bool StatisticsConfiguration::isBeginningOfYear() const {
    return beginningOfYear_;
}

const StatisticsOptions& StatisticsConfiguration::options() const {
    return opt_;
}

long StatisticsConfiguration::date() const {
    return date_;
}
long StatisticsConfiguration::time() const {
    return time_;
}
long StatisticsConfiguration::level() const {
    return level_;
}
long StatisticsConfiguration::timeStep() const {
    return timeStep_;
}
long StatisticsConfiguration::step() const {
    return step_;
}

std::string StatisticsConfiguration::param() const {
    return param_;
}
std::string StatisticsConfiguration::levType() const {
    return levType_;
}
std::string StatisticsConfiguration::gridType() const {
    return gridType_;
}
std::string StatisticsConfiguration::precision() const {
    return precision_;
}

bool StatisticsConfiguration::bitmapPresent() const {
    return missingValue_.has_value();
}

double StatisticsConfiguration::missingValue() const {
    if (missingValue_) {
        return *missingValue_;
    }
    return std::numeric_limits<double>::quiet_NaN();  // TODO: Remove fake value
}

const std::string& StatisticsConfiguration::key() const {
    return key_;
}

}  // namespace multio::action::statistics_mtg2
