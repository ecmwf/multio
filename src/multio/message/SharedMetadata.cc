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

SharedMetadata::SharedMetadata() : metadata_{std::make_shared<Metadata>()}, moveOrCopy_{false} {}

SharedMetadata::SharedMetadata(Metadata&& m, bool moveOrCopy) :
    metadata_{std::make_shared<Metadata>(std::move(m))}, moveOrCopy_{moveOrCopy} {}

SharedMetadata::SharedMetadata(const Metadata& m, bool moveOrCopy) :
    metadata_{std::make_shared<Metadata>(m)}, moveOrCopy_{moveOrCopy} {}

SharedMetadata::SharedMetadata(std::shared_ptr<Metadata> m, bool moveOrCopy) :
    metadata_{std::move(m)}, moveOrCopy_{moveOrCopy} {}


const Metadata& SharedMetadata::read() const {
    return *metadata_;
}

Metadata& SharedMetadata::modify() {
    if (moveOrCopy_ && metadata_.use_count() != 1) {
        metadata_ = std::make_shared<Metadata>(*metadata_);
        moveOrCopy_ = false;
    }
    return *metadata_;
}


void SharedMetadata::acquire() {
    moveOrCopy_ = true;
}

SharedMetadata SharedMetadata::moveOrCopy() const {
    return SharedMetadata{metadata_, true};
}

std::weak_ptr<Metadata> SharedMetadata::weakRef() const {
    return metadata_;
}


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::message

