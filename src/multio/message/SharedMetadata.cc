/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Jan 2024


#include "multio/message/SharedMetadata.h"

namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

SharedMetadata::SharedMetadata() : metadata_{std::make_shared<Metadata>()}, stealOrCopy_{false} {}

SharedMetadata::SharedMetadata(Metadata&& m, bool stealOrCopy) :
    metadata_{std::make_shared<Metadata>(std::move(m))}, stealOrCopy_{stealOrCopy} {}

SharedMetadata::SharedMetadata(const Metadata& m, bool stealOrCopy) :
    metadata_{std::make_shared<Metadata>(m)}, stealOrCopy_{stealOrCopy} {}

SharedMetadata::SharedMetadata(std::shared_ptr<Metadata> m, bool stealOrCopy) :
    metadata_{std::move(m)}, stealOrCopy_{stealOrCopy} {}


const Metadata& SharedMetadata::read() const {
    return *metadata_;
}

Metadata& SharedMetadata::modify() {
    if (stealOrCopy_ && metadata_.use_count() != 1) {
        metadata_ = std::make_shared<Metadata>(*metadata_);
    }
    return *metadata_;
}


void SharedMetadata::acquire() {
    stealOrCopy_ = true;
}

SharedMetadata SharedMetadata::stealOrCopy() const {
    return SharedMetadata{metadata_, true};
}

std::weak_ptr<Metadata> SharedMetadata::weakRef() const {
    return metadata_;
}


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::message

