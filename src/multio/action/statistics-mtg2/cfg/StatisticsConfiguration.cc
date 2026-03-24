#include "StatisticsConfiguration.h"

#include <limits.h>
#include <unistd.h>

#include <cstdint>
#include <optional>
#include <string>

#include "eckit/exception/Exceptions.h"

#include "multio/action/statistics-mtg2/cfg/StatisticsOptions.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/Glossary.h"
#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/core/EntryParser.h"

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

std::optional<std::int64_t> readTimespan(const message::Metadata& md) {
    if (const auto& timespan = dm::parseEntry(dm::TIMESPAN, md); timespan.isSet()) {
        return timespan.get().toHours();
    }
    return std::nullopt;
}

std::int64_t readParam(const message::Metadata& md) {
    // TODO: use whole validated keyset...
    if (const auto& param = dm::parseEntry(dm::PARAM, md); param.isSet()) {
        return param.get().id();
    }
    else if (auto paramId = md.getOpt<std::int64_t>(dm::legacy::ParamId); paramId) {
        return *paramId;
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

OutputTimeReference readOutputTimeReference(const message::Metadata& md, const StatisticsOptions& opt) {
    // Check if output-time-reference has been explicitly overwritten
    if (auto parsedOutRef = opt.outputTimeReference(); parsedOutRef) {
        return *parsedOutRef;
    }

    // Look up for stream in metadata or in configuration
    std::optional<std::string> stream = md.getOpt<std::string>("stream");
    if (!stream) {
        // Look for stream in options
        const auto& omd = opt.setMetadata();
        auto it = std::find_if(omd.begin(), omd.end(), [](const auto& pair) { return pair.first == "stream"; });

        if (it != omd.end()) {
            stream = it->second;
        }
    }

    // Hard defaults for given streams
    static const std::vector<std::pair<std::string, OutputTimeReference>> outputTimeRefForStream{
        {{"clmn", OutputTimeReference::StartOfWindow}, {"clte", OutputTimeReference::StartOfWindow}}};

    // If stream is given, use hard defaults
    if (stream) {
        auto it = std::find_if(outputTimeRefForStream.begin(), outputTimeRefForStream.end(),
                               [&](const auto& pair) { return pair.first == *stream; });

        if (it != outputTimeRefForStream.end()) {
            return it->second;
        }
    }

    // Return general default
    return OutputTimeReference::StartOfForecast;
}


StatisticsConfiguration::StatisticsConfiguration(const message::Metadata& md, const message::Peer& src,
                                                 const StatisticsOptions& opt) :
    opt_{opt},
    date_{readDate(md)},
    time_{readTime(md)},
    level_{readLevel(md)},
    timeStep_{readTimeStep(md, opt.timeStep())},
    step_{readStep(md)},
    timespan_{readTimespan(md)},
    param_{readParam(md)},
    levType_{readLevType(md)},
    gridType_{readGridType(md)},
    precision_{readPrecision(md)},
    missingValue_{readMissingValue(md)},
    key_{generateKey(src)},
    epoch_{computeEpoch()},
    curr_{computeCurr()},
    outputTimeReference_{readOutputTimeReference(md, opt)} {}

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
    return epoch() + static_cast<eckit::Second>(std::max(step_, static_cast<int64_t>(0)) * timeStep_);
}


const StatisticsOptions& StatisticsConfiguration::options() const {
    return opt_;
}

std::int64_t StatisticsConfiguration::date() const {
    return date_;
}
std::int64_t StatisticsConfiguration::time() const {
    return time_;
}
std::int64_t StatisticsConfiguration::timeStep() const {
    return timeStep_;
}
std::int64_t StatisticsConfiguration::step() const {
    return step_;
}
std::optional<std::int64_t> StatisticsConfiguration::timespan() const {
    return timespan_;
}

int64_t StatisticsConfiguration::param() const {
    return param_;
}
const std::string& StatisticsConfiguration::precision() const {
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

const eckit::DateTime& StatisticsConfiguration::epoch() const {
    return epoch_;
}
const eckit::DateTime& StatisticsConfiguration::curr() const {
    return curr_;
}

OutputTimeReference StatisticsConfiguration::outputTimeReference() const {
    return outputTimeReference_;
}

}  // namespace multio::action::statistics_mtg2
