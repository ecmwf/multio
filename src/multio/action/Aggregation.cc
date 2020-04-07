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
#include "multio/domain/Mappings.h"

namespace multio {
namespace action {

Aggregation::Aggregation(const eckit::Configuration& config) : Action(config) {}

bool Aggregation::doExecute(Message& msg) const {
    ScopedTimer timer{timing_};

    if (msg.tag() == Message::Tag::Field) {
        return handleField(msg);
    }

    if (msg.tag() == Message::Tag::StepComplete) {
        return handleFlush(msg);
    }

    return true;
}

bool Aggregation::handleField(Message& msg) const {
    messages_[msg.fieldId()].push_back(msg);
    return allPartsArrived(msg) ? createGlobalField(msg) : false;
}

bool Aggregation::handleFlush(const Message& msg) const {
    eckit::Log::debug<LibMultio>() << "*** Aggregating flush messages: " << *this << std::endl;

    // Initialise if need be
    if (flushes_.find(msg.domain()) == end(flushes_)) {
        flushes_[msg.domain()] = 0;
    }

    return ++flushes_.at(msg.domain()) == msg.domainCount();
}

bool Aggregation::allPartsArrived(const Message& msg) const {
    return (msg.domainCount() == messages_.at(msg.fieldId()).size()) &&
           (msg.domainCount() == domain::Mappings::instance().get(msg.domain()).size());
}

bool Aggregation::createGlobalField(Message& msgOut) const {
    const auto& fid = msgOut.fieldId();

    eckit::Buffer glField{msgOut.globalSize() * sizeof(double)};
    for (auto msg : messages_.at(fid)) {
        domain::Mappings::instance()
            .get(msg.domain())
            .at(msg.source())
            ->to_global(msg.payload(), glField);
    }

    msgOut.payload() = std::move(glField);

    messages_.erase(fid);

    return true;
}

void Aggregation::print(std::ostream& os) const {
    os << "Aggregation()";
}


static ActionBuilder<Aggregation> AggregationBuilder("Aggregation");

}  // namespace action
}  // namespace multio
