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

#include "sandbox/Mappings.h"
#include "sandbox/print_buffer.h"

namespace multio {
namespace sandbox {
namespace actions {

Aggregation::Aggregation(const eckit::Configuration& config) :
    Action(config),
    map_name_(config.getString("mapping")) {}

bool Aggregation::execute(Message message) {

    auto field_id = message.field_id();
    messages_[field_id].push_back(message);

    eckit::Log::info() << "  ---  Aggregation action is being executed" << std::endl
                       << "          -- no_maps: " << Mappings::instance().get(map_name_).size()
                       << ", no_fields: " << messages_.at(field_id).size() << std::endl;

    // All parts arrived?
    bool ret = messages_.at(field_id).size() == Mappings::instance().get(map_name_).size();
    if (ret) {
        auto global_field_size = message.field_size();
        std::vector<double> global_field(global_field_size);
        for (auto msg : messages_.at(field_id)) {
            auto data_ptr = static_cast<double*>(msg.payload().data());
            std::vector<double> local_field(data_ptr, data_ptr + msg.size() / sizeof(double));

            auto ii = 0;
            for (auto idx : Mappings::instance().get(map_name_).at(msg.source())) {
                global_field[idx] = local_field[ii++];
            }
        }

        eckit::Log::info() << "          -- print aggregated field: " << std::flush;
        print_buffer(global_field, eckit::Log::info(), " ");
        eckit::Log::info() << std::endl;

        // Yet another unnecessary copy
        message.payload() = eckit::Buffer{reinterpret_cast<char*>(global_field.data()),
                                          global_field_size * sizeof(double)};
    }
    return ret;
}

void Aggregation::print(std::ostream& os) const {
    os << "Aggregation()";
}


static ActionBuilder<Aggregation> AggregationBuilder("Aggregation");

}  // namespace actions
}  // namespace sandbox
}  // namespace multio
