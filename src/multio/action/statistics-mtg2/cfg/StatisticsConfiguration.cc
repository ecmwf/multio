#include "StatisticsConfiguration.h"

#include <limits.h>
#include <unistd.h>

#include <cstdint>
#include <optional>
#include <string>

#include "eckit/exception/Exceptions.h"

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/Glossary.h"
#include "multio/datamod/MarsKeys.h"

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
    key_{generateKey(src)},
    epoch_{computeEpoch()},
    curr_{computeCurr()} {}

StatisticsConfiguration::StatisticsConfiguration(const message::Message& msg, const StatisticsOptions& opt) :
    StatisticsConfiguration(msg.metadata(), msg.source(), opt) {};


std::string StatisticsConfiguration::generateKey(const message::Peer& src) const {
    std::ostringstream os;
    os << param_ << "-" << level_ << "-" << levType_ << "-" << gridType_ << "-" << precision_ << "-" << src.group()
       << "_" << src.id();
    return os.str();
}

eckit::DateTime StatisticsConfiguration::computeEpoch() const {
    eckit::Date date{date_};
    const auto hour = time_ / 10000;
    const auto minute = (time_ % 10000) / 100;
    return eckit::DateTime{date, eckit::Time{hour, minute, 0}};
}


eckit::DateTime StatisticsConfiguration::computeCurr() const {
    return epoch() + static_cast<eckit::Second>(std::max(step_, 0L) * timeStep_);
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
    throw eckit::SeriousBug("Missing value is undefined!", Here());
}

const std::string& StatisticsConfiguration::key() const {
    return key_;
}

eckit::DateTime StatisticsConfiguration::epoch() const {
    return epoch_;
}
eckit::DateTime StatisticsConfiguration::curr() const {
    return curr_;
}

}  // namespace multio::action::statistics_mtg2
