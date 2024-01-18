/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Mask.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/domain/Mappings.h"

namespace multio {
namespace domain {

Mask& Mask::instance() {
    static Mask singleton;
    return singleton;
}

std::string Mask::key(const message::Metadata& md) {
    return "(" + md.get<std::string>("domain") + "," + std::to_string(md.get<std::int64_t>("level")) + ")";
}

void Mask::add(message::Message msg) {
    std::lock_guard<std::mutex> lock{mutex_};

    addPartialMask(msg);

    if (allPartsArrived(msg)) {
        createBitmask(msg);
    }
}

// const std::vector<bool>& Mask::get(const std::string& bkey) const {
EncodedRunLengthPayload Mask::get(const std::string& bkey) const {
    if (bitmasks_.find(bkey) == std::end(bitmasks_)) {
        throw eckit::AssertionFailed("There is no bitmask for " + bkey);
    }

    return EncodedRunLengthPayload{bitmasks_.at(bkey)};
}

void Mask::addPartialMask(message::Message msg) {
    // Using a lookup table for sanity check

    auto& msgList = messages_[msg.fieldId()];

    msgList.push_back(std::move(msg));
}

bool Mask::allPartsArrived(const message::Message& msg) const {
    const auto& domainMap = domain::Mappings::instance().get(msg.domain());

    return domainMap.isComplete() && (messages_.at(msg.fieldId()).size() == domainMap.size());
}

void Mask::createBitmask(message::Message inMsg) {
    const auto& fid = inMsg.fieldId();

    std::vector<bool> bitmask;
    bitmask.resize(inMsg.globalSize());
    // Important note: Resize is also value initializing the std::vector to 0.
    // This means by default all fields are masked. This is important, especially for
    // partial aggregation when some nodes are not even set up and do not send masks at all.
    for (const auto& msg : messages_.at(fid)) {
        domain::Mappings::instance().get(msg.domain()).at(msg.source())->toBitmask(msg, bitmask);
    }

    // Assert invariants such are bound to be creating this the first and last time
    auto bkey = Mask::key(inMsg.metadata());
    // bitmasks_[bkey] = std::move(bitmask);
    bitmasks_.emplace(bkey, encodeMaskRunLength(bitmask, bitmask.size()));

    messages_.at(inMsg.fieldId()).clear();
}

}  // namespace domain
}  // namespace multio
