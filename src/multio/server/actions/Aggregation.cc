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

#include "multio/LibMultio.h"
#include "multio/server/Mappings.h"

namespace multio {
namespace server {
namespace actions {

Aggregation::Aggregation(const eckit::Configuration& config) : Action(config) {}

void Aggregation::execute(Message msg) const {

    if (msg.tag() == Message::Tag::Field) {
        auto field_id = msg.field_id();
        auto map_name = msg.mapping();
        messages_[field_id].push_back(msg);

        // All parts arrived?
        bool ret = messages_.at(field_id).size() == msg.map_count();
        ret &= Mappings::instance().get(map_name).size() == msg.map_count();
        if (!ret) {
            return;
        }

        eckit::Buffer global_field(msg.field_size() * sizeof(double));
        for (auto m : messages_.at(field_id)) {
            Mappings::instance().get(map_name).at(m.source())->to_global(m.payload(), global_field);
        }

        msg.payload() = std::move(global_field);

        messages_.erase(field_id);

        if (next_) {  // May want to assert next_
            next_->execute(msg);
        }
    }

    if (msg.tag() == Message::Tag::StepComplete) {

        eckit::Log::debug<LibMultio>() << "*** Aggregating flush messages: " << *this << std::endl;

        // Initialise
        if(flushes_.find(msg.mapping()) == end(flushes_)) {
            flushes_[msg.mapping()] = 0;
        }

        if (++flushes_.at(msg.mapping()) != msg.map_count()) {
            return;
        }

        if (next_) {  // May want to assert next_
            next_->execute(msg);
        }
    }

}

void Aggregation::print(std::ostream& os) const {
    os << "Aggregation()";
}


static ActionBuilder<Aggregation> AggregationBuilder("Aggregation");

}  // namespace actions
}  // namespace server
}  // namespace multio
