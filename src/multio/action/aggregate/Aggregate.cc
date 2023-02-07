/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Aggregate.h"

#include <algorithm>

#include "multio/LibMultio.h"
#include "multio/domain/Mappings.h"
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace action {

using message::Peer;

Aggregate::Aggregate(const ConfigurationContext& confCtx) : ChainedAction(confCtx) {}

void Aggregate::executeImpl(Message msg) {

    eckit::Log::info() << " ***** Aggregating message " << msg << std::endl;

    if ((msg.tag() == Message::Tag::Field) && handleField(msg)) {
        executeNext(globalField(msg.fieldId()));
    }

    if ((msg.tag() == Message::Tag::StepComplete) && handleFlush(msg)) {
        executeNext(globalFlush(msg.fieldId()));
    }
}

bool Aggregate::handleField(const Message& msg) {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
    if (not aggCatalogue_.contains(msg.fieldId())) {
        aggCatalogue_.addNew(msg);
    }
    // TODO: Perhaps call collect indices here and store it for a later call on check consistnecy
    domain::Mappings::instance()
        .get(msg.domain())
        .at(msg.source())
        ->toGlobal(msg, aggCatalogue_.getMessage(msg.fieldId()));
    aggCatalogue_.bookProcessedPart(msg.fieldId(), msg.source());
    return allPartsArrived(msg);
}

auto Aggregate::flushCount(const Message& msg) {

    auto res = flushes_[msg.fieldId()].emplace(msg.source());

    if (not res.second) {
        std::ostringstream os;
        os << "Flush message " << msg << " has already been received";
        throw eckit::UserError(os.str(), Here());
    }

    return flushes_.at(msg.fieldId()).size();
}

bool Aggregate::handleFlush(const Message& msg) {
    // Initialise if need be
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    const auto& domainMap = domain::Mappings::instance().get(msg.domain());
    auto flCount = flushCount(msg);

    return domainMap.isComplete() && flCount == domainMap.size();
}

bool Aggregate::allPartsArrived(const Message& msg) const {
    LOG_DEBUG_LIB(LibMultio) << " *** Number of messages for field " << msg.fieldId() << " are "
                             << aggCatalogue_.partsCount(msg.fieldId()) << std::endl;

    const auto& domainMap = domain::Mappings::instance().get(msg.domain());

    return domainMap.isComplete() && (aggCatalogue_.partsCount(msg.fieldId()) == domainMap.size());
}

Message Aggregate::globalField(const std::string& fid) {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    // TODO: checking domain consistency is skipped for now...
    // domain::Mappings::instance().checkDomainConsistency(messages_.at(fid));

    return aggCatalogue_.extract(fid);
}

Message Aggregate::globalFlush(const std::string &fid)
{
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    auto flush = flushes_.extract(fid);

    return Message{{Message::Tag::StepComplete, Peer{}, Peer{}, std::string(fid)}};
}

void Aggregate::print(std::ostream& os) const {
    os << "Aggregate(for " << aggCatalogue_.size() << " fields = [" << aggCatalogue_ << "])";
}


static ActionBuilder<Aggregate> AggregateBuilder("aggregate");

}  // namespace action
}  // namespace multio

