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
#include "multio/message/Message.h"

namespace multio {
namespace domain {

namespace  {
std::string partialMaskId(const message::Message& msg) {
    std::ostringstream os;
    os << msg;
    return os.str();
}
}  // namespace

Mask& Mask::instance() {
    static Mask singleton;
    return singleton;
}

std::string Mask::key(const message::Metadata& md) {
    return "(" + md.getString("domain") + "," + std::to_string(md.getLong("level")) + ")";
}

void Mask::add(message::Message msg) {
    std::lock_guard<std::mutex> lock{mutex_};

    // This is sub-optimal but it does not matter because it happens at startup
    // Using a lookup table instead would also be fine

    eckit::Log::info() << " *** Checking mask " << msg << std::endl;
    const auto& msgList = messages_[msg.fieldId()];
    const auto& maskId = partialMaskId(msg);
    if (std::find_if(begin(msgList), end(msgList), [&maskId](const message::Message& m) {
            return partialMaskId(m) == maskId;
        }) == end(msgList)) {
        eckit::Log::info() << " *** Adding mask " << msg << std::endl;
        messages_.at(msg.fieldId()).push_back(msg);
    }

    if (not allPartsArrived(msg)) {
        return;
    }

    createBitmask(msg);
}

const std::vector<bool>& Mask::get(const std::string& bkey) const {
    if (bitmasks_.find(bkey) == std::end(bitmasks_)) {
        throw eckit::AssertionFailed("There is no bitmask for " + bkey);
    }

    return bitmasks_.at(bkey);
}

bool Mask::allPartsArrived(message::Message msg) const {
    eckit::Log::info() << " *** Domain cout         : " << msg.domainCount() << std::endl;
    eckit::Log::info() << " *** Message-list size   : "
                       << messages_.at(msg.fieldId()).size() << std::endl;
    eckit::Log::info() << " *** Stored domain count : "
                       << domain::Mappings::instance().get(msg.domain()).size()
                       << std::endl;
    return (msg.domainCount() == messages_.at(msg.fieldId()).size()) &&
           (msg.domainCount() == domain::Mappings::instance().get(msg.domain()).size());
}

void Mask::createBitmask(message::Message inMsg) {
    const auto& fid = inMsg.fieldId();

    std::vector<bool> bitmask;
    bitmask.resize(inMsg.globalSize());
    for (const auto& msg : messages_.at(fid)) {
        domain::Mappings::instance().get(msg.domain()).at(msg.source())->to_bitmask(msg, bitmask);
    }

    // Assert invariants such are bound to be creating this the first and last time
    auto bkey = Mask::key(inMsg.metadata());
    eckit::Log::info() << "Creating bitmask for " << bkey << std::endl;
    bitmasks_[bkey] = std::move(bitmask);

    messages_.at(inMsg.fieldId()).clear();
}

}  // namespace domain
}  // namespace multio
