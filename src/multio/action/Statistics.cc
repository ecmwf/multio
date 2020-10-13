/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Statistics.h"

#include <algorithm>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"
#include "multio/action/TemporalStatistics.h"

namespace multio {
namespace action {

namespace  {

const std::map<const char, const std::string> symbol_to_unit{
    {'h', "hour"}, {'d', "day"}, {'m', "month"}};

std::string set_unit(std::string const& output_freq) {
    const auto& symbol = output_freq.back();

    if (symbol_to_unit.find(symbol) == end(symbol_to_unit)) {
        throw eckit::SeriousBug{"Time unit for symbol " + std::string{symbol} +
                                " is not supported"};
    }
    return symbol_to_unit.at(symbol);
}

long set_frequency(const std::string& output_freq) {
    auto freq = output_freq.substr(0, output_freq.size() - 1);
    return std::stol(freq);
}
}  // namespace

Statistics::Statistics(const eckit::Configuration& config) :
    Action{config},
    timeUnit_{set_unit(config.getString("output_frequency"))},
    timeSpan_{set_frequency(config.getString("output_frequency"))},
    operations_{config.getStringVector("operations")} {}

void Statistics::execute(message::Message msg) const {
    ScopedTimer timer{timing_};

    LOG_DEBUG_LIB(LibMultio) << "         Metadata: " << msg.metadata() << std::endl;

    if(fieldStats_.find(msg.name()) == end(fieldStats_)) {
        fieldStats_[msg.name()] = TemporalStatistics::build(timeUnit_, timeSpan_, operations_, msg);
    }

    if(fieldStats_.at(msg.name())->process(msg)) {
        return;
    }

    auto md = msg.metadata();
    for (auto&& stat : fieldStats_.at(msg.name())->compute(msg)) {
        md.set("operation", stat.first);
        message::Message newMsg{
            message::Message::Header{message::Message::Tag::Statistics, message::Peer{},
                                     message::Peer{}, msg.name(), msg.category(), msg.domainCount(),
                                     msg.globalSize(), msg.domain(), message::to_string(md)},
            std::move(stat.second)};

        executeNext(newMsg);
    }

    fieldStats_.at(msg.name())->reset(msg);
}

void Statistics::print(std::ostream& os) const {
    os << "Statistics(output frequency = " << timeSpan_ << ", unit = " << timeUnit_
       << ", operations = ";
    bool first = true;
    for (const auto& ops : operations_) {
        os << (first ? "" : ", ");
        os << ops;
        first = false;
    }
    os << ")";
}


static ActionBuilder<Statistics> StatisticsBuilder("Statistics");

}  // namespace action
}  // namespace multio
