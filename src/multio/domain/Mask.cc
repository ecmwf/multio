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

void Mask::add(message::Message inMsg) {
    std::lock_guard<std::mutex> lock{mutex_};

    messages_[inMsg.fieldId()].push_back(inMsg);
    if (not allPartsArrived(inMsg)) {
        return;
    }

    const auto& fid = inMsg.fieldId();

    eckit::Log::info() << "Allocating dynamic bitmap (bitmask)" << std::endl;
    std::vector<uint8_t> bitmask;
    eckit::Log::info() << "Resizing dynamic bitmap (bitmask)" << std::endl;
    bitmask.resize(inMsg.globalSize());
    eckit::Log::info() << "Allocated dynamic bitmap (bitmask)" << std::endl;
    for (const auto& msg : messages_.at(fid)) {
        eckit::Log::info() << "Aggregate message " << msg.source() << ": " << msg.metadata()
                           << std::endl;
        domain::Mappings::instance().get(msg.domain()).at(msg.source())->to_bitmask(msg, bitmask);
    }

    // Assert invariants such are bound to be creating this the first and last time
    // TODO: Do not use fid, just use the mask name and the level
    auto bkey = "(" + inMsg.name() + "," + std::to_string(inMsg.metadata().getLong("level")) + ")";
    eckit::Log::info() << "Bitmap key: " << bkey << std::endl;
    bitmasks_[bkey] = std::move(bitmask);
    eckit::Log::info() << "Number of bitmaps: " << bitmasks_.size() << std::endl;
    eckit::Log::info() << "Size of bitmap " << bkey << " : " << bitmasks_.at(bkey).size()
                       << std::endl;

    // messages_.at(inMsg.fieldId()).clear();
}

const std::vector<uint8_t>& Mask::get(const std::string& bkey) const {
    if (bitmasks_.find(bkey) == std::end(bitmasks_)) {
        throw eckit::AssertionFailed("There is no bitmask for " + bkey);
    }

    return bitmasks_.at(bkey);
}

bool Mask::allPartsArrived(message::Message msg) const {
    return (msg.domainCount() == messages_.at(msg.fieldId()).size()) &&
           (msg.domainCount() == domain::Mappings::instance().get(msg.domain()).size());
}

}  // namespace domain
}  // namespace multio
