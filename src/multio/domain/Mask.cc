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

Mask& Mask::instance() {
    static Mask singleton;
    return singleton;
}

std::string Mask::key(const message::Metadata& md) {
    return "(" + md.getString("domain") + "," + std::to_string(md.getLong("level")) + ")";
}

void Mask::add(message::Message msg) {
    std::lock_guard<std::mutex> lock{mutex_};

    messages_[msg.fieldId()].push_back(msg);
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
    return (msg.domainCount() == messages_.at(msg.fieldId()).size()) &&
           (msg.domainCount() == domain::Mappings::instance().get(msg.domain()).size());
}

void Mask::createBitmask(message::Message inMsg) {
    const auto& fid = inMsg.fieldId();

    eckit::Log::info() << "Allocating dynamic bitmap (bitmask)" << std::endl;
    std::vector<bool> bitmask;
    eckit::Log::info() << "Resizing dynamic bitmap (bitmask)" << std::endl;
    bitmask.resize(inMsg.globalSize());
    eckit::Log::info() << "Allocated dynamic bitmap (bitmask)" << std::endl;
    for (const auto& msg : messages_.at(fid)) {
        eckit::Log::info() << "Aggregate message " << msg.source() << ": " << msg.metadata()
                           << std::endl;
        domain::Mappings::instance().get(msg.domain()).at(msg.source())->to_bitmask(msg, bitmask);
    }

    // Assert invariants such are bound to be creating this the first and last time
    auto bkey = Mask::key(inMsg.metadata());
    bitmasks_[bkey] = std::move(bitmask);
    eckit::Log::info() << "Number of bitmaps: " << bitmasks_.size() << std::endl;
    eckit::Log::info() << "Size of bitmap " << bkey << ": " << bitmasks_.at(bkey).size()
                       << " -- Number of land points: "
                       << std::count(std::begin(bitmasks_.at(bkey)), std::end(bitmasks_.at(bkey)),
                                     false)
                       << std::endl;

    messages_.at(inMsg.fieldId()).clear();
}

}  // namespace domain
}  // namespace multio
