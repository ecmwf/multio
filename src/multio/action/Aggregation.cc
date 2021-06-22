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
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace action {

Aggregation::Aggregation(const eckit::Configuration& config) : Action(config) {}

void Aggregation::execute(Message msg) const {
    if ((msg.tag() == Message::Tag::Field) && handleField(msg)) {
        executeNext(createGlobalField(msg));
    }

    if ((msg.tag() == Message::Tag::StepComplete) && handleFlush(msg)) {
        executeNext(msg);
    }
}

bool Aggregation::handleField(const Message& msg) const {
    eckit::AutoTiming timing{statistics_.timer_, statistics_.actionTiming_};
    messages_[msg.fieldId()].push_back(msg);
    return allPartsArrived(msg);
}

bool Aggregation::handleFlush(const Message& msg) const {
    // Initialise if need be
    eckit::AutoTiming timing{statistics_.timer_, statistics_.actionTiming_};
    if (flushes_.find(msg.domain()) == end(flushes_)) {
        flushes_[msg.domain()] = 0;
    }

    return ++flushes_.at(msg.domain()) == msg.domainCount();
}

bool Aggregation::allPartsArrived(const Message& msg) const {
  LOG_DEBUG_LIB(LibMultio) << " *** Number of messages for field " << msg.fieldId()
                           << " are " << messages_.at(msg.fieldId()).size() << std::endl;

  return (msg.domainCount() == messages_.at(msg.fieldId()).size()) &&
         (msg.domainCount() == domain::Mappings::instance().get(msg.domain()).size());
}

Message Aggregation::createGlobalField(const Message& msg) const {
    eckit::AutoTiming timing{statistics_.timer_, statistics_.actionTiming_};

    const auto& fid = msg.fieldId();
    LOG_DEBUG_LIB(LibMultio) << " *** Creating global field for " << fid << std::endl;

    auto levelCount = msg.metadata().getLong("levelCount", 1);

    Message msgOut{Message::Header{msg.header()},
                   eckit::Buffer{msg.globalSize() * levelCount * sizeof(double)}};

    for (const auto& msg : messages_.at(fid)) {
        domain::Mappings::instance().get(msg.domain()).at(msg.source())->to_global(msg, msgOut);
    }

    messages_.erase(fid);

    return msgOut;
}

void Aggregation::print(std::ostream& os) const {
    os << "Aggregation(for " << messages_.size() << " fields = [";
    for (const auto& msg : messages_) {
        os << '\n' << "  --->  " << msg.first;
    }
    os << "])";
}


static ActionBuilder<Aggregation> AggregationBuilder("Aggregation");

}  // namespace action
}  // namespace multio
