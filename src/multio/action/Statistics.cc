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

namespace multio {
namespace action {

namespace  {
std::string set_unit(std::string const& output_freq) {
    const auto& unit = output_freq.back();

    if (unit == 'h') {
        return "hour";
    }

    if (unit == 'd') {
        return "day";
    }

    if (unit == 'm') {
        return "month";
    }

    throw eckit::SeriousBug{"Time unit " + std::string{unit} + " is not supported"};
}

long set_frequency(const std::string& output_freq) {
    auto freq = output_freq.substr(0, output_freq.size() - 1);
    return std::stol(freq);
}
}  // namespace

Statistics::Statistics(const eckit::Configuration& config) :
    Action{config},
    timeUnit_{set_unit(config.getString("output_frequency"))},
    writeFrequency_{set_frequency(config.getString("output_frequency"))},
    operations_{config.getStringVector("operations")} {}

void Statistics::doExecute(message::Message msg) const {
    ScopedTimer timer{timing_};

    for (const auto& ops : operations_) {
        applyOperation(ops);
    }

    if (msg.metadata().getUnsigned("step") % writeFrequency_ != 0) {
        return;
    }

    executeNext(msg);
}

void Statistics::print(std::ostream& os) const {
    os << "Statistics(output frequency = " << writeFrequency_ << ", operations = ";
    bool first = true;
    for (const auto& ops : operations_) {
        os << (first ? "" : ", ");
        os << ops;
        first = false;
    }
    os << ")";
}

void Statistics::applyOperation(const std::string&) const {
    [](){}(); // TODO: Call a dictionary of functions;
    return;
}


static ActionBuilder<Statistics> StatisticsBuilder("Statistics");

}  // namespace action
}  // namespace multio
