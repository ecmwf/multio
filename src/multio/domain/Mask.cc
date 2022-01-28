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

void Mask::add(message::Message msg) {
    messages_[msg.fieldId()].push_back(msg);
    if (not allPartsArrived(msg)) {
        return;
    }

    const auto& fid = msg.fieldId();

    std::vector<bool> bitmask(msg.globalSize());
    for (const auto& msg : messages_.at(fid)) {
        domain::Mappings::instance().get(msg.domain()).at(msg.source())->to_bitmask(msg, bitmask);
    }

    // Assert invariantssuch are bound to be creating this the first and last time
    // TODO: Do not use fid, just use the mask name and the level
    bitmasks_[fid].emplace(std::move(bitmask));
}

bool Mask::allPartsArrived(message::Message msg) {
}
