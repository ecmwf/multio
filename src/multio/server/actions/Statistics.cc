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
namespace server {
namespace actions {

Statistics::Statistics(const eckit::Configuration& config) :
    Action{config},
    writeFrequency_{config.getUnsigned("output_frequency")},
    operations_{config.getStringVector("operations")} {}

void Statistics::execute(Message msg) const {
    {
        ScopedTimer timer{timing_};

        for (const auto& ops : operations_) {
            applyOperation(ops);
        }

        if (msg.metadata().getUnsigned("istep") % writeFrequency_ != 0) {
            return;
        }

        ASSERT(next_);
    }

    // TODO: For loop to return from all statistics
    next_->execute(msg); // This is for instant operation only
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

}  // namespace actions
}  // namespace server
}  // namespace multio
