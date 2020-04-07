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

#include "multio/LibMultio.h"

namespace multio {
namespace action {

Statistics::Statistics(const eckit::Configuration& config) :
    Action{config},
    writeFrequency_{config.getUnsigned("output_frequency")},
    operations_{config.getStringVector("operations")} {}

bool Statistics::doExecute(message::Message& msg) const {
    ScopedTimer timer{timing_};

    for (const auto& ops : operations_) {
        applyOperation(ops);
    }

    if (msg.metadata().getUnsigned("istep") % writeFrequency_ != 0) {
        return false;
    }

    return true;
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

void Statistics::applyOperation(const std::string& name) const {
    [](){}(); // TODO: Call a dictionary of functions;
    return;
}


static ActionBuilder<Statistics> StatisticsBuilder("Statistics");

}  // namespace action
}  // namespace multio
