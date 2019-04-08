/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Aggregation.h"

#include <algorithm>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"

#include "sandbox/Aggregator.h"
#include "sandbox/Mappings.h"
#include "sandbox/print_buffer.h"

namespace multio {
namespace sandbox {
namespace actions {

Aggregation::Aggregation(const eckit::Configuration& config) :
    Action(config),
    map_name_(config.getString("mapping")) {}

void Aggregation::execute(Message msg) const {

    auto field_id = msg.field_id();
    messages_[field_id].push_back(msg);

    eckit::Log::info() << "  ---  Aggregation action is being executed" << std::endl
                       << "          -- no_maps: " << Mappings::instance().get(map_name_).size()
                       << ", no_fields: " << messages_.at(field_id).size() << std::endl;

    // All parts arrived?
    bool ret = messages_.at(field_id).size() == msg.map_count();
    ret &= Mappings::instance().get(map_name_).size() == msg.map_count();
    if (!ret) {
        return;
    }

    Aggregator agg{msg.field_size(), messages_.at(field_id).size()};

    msg.payload() = agg.gather(messages_.at(field_id), Mappings::instance().get(map_name_));

    messages_.erase(field_id);

    eckit::Log::info() << "          -- print aggregated field " << msg.field_id()
                       << " (size=" << msg.field_size() << "): ";
    print_buffer(reinterpret_cast<double*>(msg.payload().data()), msg.field_size(),
                 eckit::Log::info(), " ");
    eckit::Log::info() << std::endl;

    if (next_) {  // May want to assert next_
        next_->execute(msg);
    }
}

void Aggregation::print(std::ostream& os) const {
    os << "Aggregation()";
}


static ActionBuilder<Aggregation> AggregationBuilder("Aggregation");

}  // namespace actions
}  // namespace sandbox
}  // namespace multio
