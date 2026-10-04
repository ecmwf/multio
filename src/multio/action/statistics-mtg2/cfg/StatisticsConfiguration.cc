#include "StatisticsConfiguration.h"

#include <limits.h>
#include <unistd.h>

#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>

#include "eckit/exception/Exceptions.h"

#include "multio/action/statistics-mtg2/cfg/StatisticsOptions.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/TypeParserDumper.h"

namespace multio::action::statistics_mtg2 {

namespace dm = multio::datamod;


std::int64_t deriveLevel(const FieldMetadataKeys& md) {
    if (md.levelist.isSet()) {
        return md.levelist.get();
    }
    return 0;
}

std::string deriveLevType(const FieldMetadataKeys& md) {
    return dm::DumpType<dm::LevType>::dump(md.levtype.get());
}

std::string deriveGridType(const FieldMetadataKeys& md) {
    if (md.grid.isSet()) {
        return md.grid.get();
    }
    // Truncation is always present when we are dealing with Spherical Harmonics
    if (md.truncation.isSet()) {
        return "none";
    }
    std::ostringstream os;
    os << "Cannot find grid or truncation in metadata";
    throw eckit::SeriousBug{os.str(), Here()};
}

// TODO: add a proper EntryDef for precision
std::string derivePrecision(const message::Metadata& rawMd) {
    if (auto precision = rawMd.getOpt<std::string>("misc-precision"); precision) {
        return *precision;
    }
    throw eckit::SeriousBug{"precision metadata not present", Here()};
}

std::optional<double> deriveMissingValue(const FieldMetadataKeys& md) {
    if (md.missingValue.isSet() && md.bitmapPresent.isSet() && md.bitmapPresent.get()) {
        return md.missingValue.get();
    }
    return std::nullopt;
}

OutputTimeReference readOutputTimeReference(const FieldMetadataKeys& md, const StatisticsOptions& opt) {
    // Check if output-time-reference has been explicitly overwritten
    if (auto parsedOutRef = opt.outputTimeReference(); parsedOutRef) {
        return *parsedOutRef;
    }

    // Look up for stream in metadata or in configuration
    std::optional<std::string> stream;
    if (md.stream.isSet()) {
        stream = md.stream.get();
    }
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
    md_{dm::readRecord<FieldMetadataKeys>(md)},
    opt_{opt},
    level_{deriveLevel(md_)},
    levType_{deriveLevType(md_)},
    gridType_{deriveGridType(md_)},
    precision_{derivePrecision(md)},
    missingValue_{deriveMissingValue(md_)},
    key_{generateKey(src)},
    epoch_{computeEpoch()},
    curr_{computeCurr()},
    outputTimeReference_{readOutputTimeReference(md_, opt)} {
    validateMetadata();
}

StatisticsConfiguration::StatisticsConfiguration(const message::Message& msg, const StatisticsOptions& opt) :
    StatisticsConfiguration(msg.metadata(), msg.source(), opt) {};


std::string StatisticsConfiguration::generateKey(const message::Peer& src) const {
    std::ostringstream os;
    os << param() << "-" << level_ << "-" << levType_ << "-" << gridType_ << "-" << precision_ << "-" << src.group()
       << "_" << src.id();
    return os.str();
}

eckit::DateTime StatisticsConfiguration::computeEpoch() const {
    eckit::Date d{date()};
    const auto hour = time() / 10000;
    const auto minute = (time() % 10000) / 100;
    return eckit::DateTime{d, eckit::Time{hour, minute, 0}};
}


eckit::DateTime StatisticsConfiguration::computeCurr() const {
    return epoch() + static_cast<eckit::Second>(std::max(stepInSeconds(), static_cast<int64_t>(0)));
}

void StatisticsConfiguration::validateMetadata() const {
    if (outputStepInSeconds() <= 0) {
        throw eckit::UserError{"outputStepInSeconds must be positive", Here()};
    }
    if (integrationStepInSeconds() <= 0) {
        throw eckit::UserError{"integrationStepInSeconds must be positive", Here()};
    }
    if (distanceFromPreviousStepInSeconds() <= 0) {
        throw eckit::UserError{"distanceFromPreviousStepInSeconds must be positive", Here()};
    }
    if (md_.stattype.isSet() && !md_.timespan.isSet()) {
        throw eckit::UserError{"stattype requires timespan", Here()};
    }
    if (md_.timespan.isSet() && !md_.timespan.get().isDuration()) {
        throw eckit::UserError{"Statistical fields require a finite timespan duration", Here()};
    }
    if (isStatistical() && !md_.timeIncrementInSeconds.isSet()) {
        throw eckit::UserError{"Statistical fields require timeIncrementInSeconds", Here()};
    }
    if (!isStatistical() && md_.timeIncrementInSeconds.isSet()) {
        throw eckit::UserError{"Instantaneous fields must not contain timeIncrementInSeconds", Here()};
    }
    if (md_.timeIncrementInSeconds.isSet() && md_.timeIncrementInSeconds.get() <= 0) {
        throw eckit::UserError{"timeIncrementInSeconds must be positive", Here()};
    }
    if (isStatistical() && opt_.windowType() == WindowType::BackwardOffset) {
        throw eckit::UserError{"Backward-offset windows only support instantaneous input", Here()};
    }
}


const StatisticsOptions& StatisticsConfiguration::options() const {
    return opt_;
}

std::int64_t StatisticsConfiguration::date() const {
    return md_.date.get();
}
std::int64_t StatisticsConfiguration::time() const {
    return md_.time.get();
}
std::int64_t StatisticsConfiguration::outputStepInSeconds() const {
    return md_.outputStepInSeconds.get();
}
std::int64_t StatisticsConfiguration::integrationStepInSeconds() const {
    return md_.integrationStepInSeconds.get();
}
std::int64_t StatisticsConfiguration::distanceFromPreviousStepInSeconds() const {
    return md_.distanceFromPreviousStepInSeconds.get();
}
std::optional<std::int64_t> StatisticsConfiguration::timeIncrementInSeconds() const {
    if (md_.timeIncrementInSeconds.isSet()) {
        return md_.timeIncrementInSeconds.get();
    }
    return std::nullopt;
}
std::int64_t StatisticsConfiguration::stepInSeconds() const {
    return md_.step.get().toSeconds();
}
std::optional<std::int64_t> StatisticsConfiguration::timespan() const {
    if (md_.timespan.isSet()) {
        return md_.timespan.get().toHours();
    }
    return std::nullopt;
}
std::optional<std::int64_t> StatisticsConfiguration::timespanInSeconds() const {
    if (md_.timespan.isSet()) {
        return md_.timespan.get().toSeconds();
    }
    return std::nullopt;
}
std::optional<dm::StatType> StatisticsConfiguration::stattype() const {
    if (md_.stattype.isSet()) {
        return md_.stattype.get();
    }
    return std::nullopt;
}
bool StatisticsConfiguration::isStatistical() const {
    return md_.timespan.isSet() || md_.stattype.isSet();
}

int64_t StatisticsConfiguration::param() const {
    return md_.param.get().id();
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
