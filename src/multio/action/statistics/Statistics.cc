/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "Statistics.h"

#include <algorithm>

#include "TemporalStatistics.h"
#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace action {

namespace {

const std::map<const char, const std::string> symbol_to_unit{{'h', "hour"}, {'d', "day"}, {'m', "month"}};

std::string set_unit(std::string const& output_freq) {
    const auto& symbol = output_freq.back();

    if (symbol_to_unit.find(symbol) == end(symbol_to_unit)) {
        throw eckit::SeriousBug{"Time unit for symbol " + std::string{symbol} + " is not supported"};
    }
    return symbol_to_unit.at(symbol);
}

long set_frequency(const std::string& output_freq) {
    auto freq = output_freq.substr(0, output_freq.size() - 1);
    return std::stol(freq);
}

}  // namespace

Statistics::Statistics(const ConfigurationContext& confCtx) :
    ChainedAction{confCtx},
    timeUnit_{set_unit(confCtx.config().getString("output-frequency"))},
    timeSpan_{set_frequency(confCtx.config().getString("output-frequency"))},
    operations_{confCtx.config().getStringVector("operations")},
    useCurrentTime_{confCtx.config().getBool("use-current-time", false)} {}

void Statistics::executeImpl(message::Message msg) {
    // Pass through -- no statistics for messages other than fields
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    auto md = msg.metadata();
    if (!md.has("startDate")) {
        md.set("startDate", md.getLong("date"));
    }
    if (!md.has("startTime")) {
        md.set("startTime", md.getLong("time"));
    }
    if (!md.has("step") || !md.has("timeStep") || !md.has("step-frequency")) {
        throw eckit::SeriousBug("MULTIO ACTION STATISTICS :: missing metadata", Here());
    }

    std::ostringstream os;
    {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

        LOG_DEBUG_LIB(multio::LibMultio) << "*** " << msg.destination() << " -- metadata: " << md << std::endl;

        // Create a unique key for the fieldStats_ map
        os << msg.metadata().getString("param", "xxx.yyy") << msg.metadata().getLong("level", 0L) << msg.source();

        if (fieldStats_.find(os.str()) == end(fieldStats_)) {
            fieldStats_[os.str()] = TemporalStatistics::build(timeUnit_, timeSpan_, operations_, msg, useCurrentTime_);
        }

        if (fieldStats_.at(os.str())->process(msg)) {
            return;
        }

        md.set("timeUnit", timeUnit_);
        auto timeSpanInHours = fieldStats_.at(os.str())->current().timeSpanInHours();
        md.set("timeSpanInHours", timeSpanInHours);
        md.set("stepRange", fieldStats_.at(os.str())->stepRange(md.getLong("step")));
        md.set("currentDate", fieldStats_.at(os.str())->current().endPoint().date().yyyymmdd());
        md.set("currentTime", fieldStats_.at(os.str())->current().endPoint().time().hhmmss());

        if (md.has("step") && md.has("timeStep")) {
            auto stepInSeconds = md.getLong("step") * md.getLong("timeStep");
            ASSERT(stepInSeconds % 3600 == 0);
            auto stepInHours = stepInSeconds / 3600;
            md.set("stepInHours", stepInHours);
            // Fix negative ranges (timespan can be bigger than step )
            auto prevStep = std::max(stepInHours - timeSpanInHours, 0L);
            auto stepRangeInHours = std::to_string(prevStep) + "-" + std::to_string(stepInHours);
            md.set("stepRangeInHours", stepRangeInHours);
        }
    }
    for (auto&& stat : fieldStats_.at(os.str())->compute(msg)) {
        md.set("operation", stat.first);
        message::Message newMsg{message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(),
                                                         message::Metadata{md}},
                                std::move(stat.second)};

        executeNext(newMsg);
    }

    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    fieldStats_.at(os.str())->reset(msg);
}

void Statistics::print(std::ostream& os) const {
    os << "Statistics(output frequency = " << timeSpan_ << ", unit = " << timeUnit_ << ", operations = ";
    bool first = true;
    for (const auto& ops : operations_) {
        os << (first ? "" : ", ");
        os << ops;
        first = false;
    }
    os << ")";
}

static ActionBuilder<Statistics> StatisticsBuilder("statistics");

}  // namespace action
}  // namespace multio
