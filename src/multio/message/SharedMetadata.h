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

#pragma once

#include "multio/message/Metadata.h"

#include <memory>


namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

class SharedMetadata {
public:
    using This = SharedMetadata;

    SharedMetadata();
    SharedMetadata(Metadata&&, bool moveOrCopy = false);
    SharedMetadata(const Metadata&, bool moveOrCopy = false);
    SharedMetadata(std::shared_ptr<Metadata>, bool moveOrCopy = false);

    SharedMetadata(const This&) = default;
    SharedMetadata(This&&) noexcept = default;

    This& operator=(const This& other) = default;
    This& operator=(This&& other) noexcept = default;

    // WARNING TO USERS: NEVER STORE THESE REFERENCES
    const Metadata& read() const;
    Metadata& modify();

    void acquire();
    SharedMetadata moveOrCopy() const;


    // Calling `weakRef` may behave differently befare and after calls to `modify`
    // because metadata can be copied lazily
    std::weak_ptr<Metadata> weakRef() const;

private:
    std::shared_ptr<Metadata> metadata_;
    bool moveOrCopy_ = false;
};

//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::message
