/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <tuple>
#include <type_traits>

#include "multio/datamod/core/Entry.h"
#include "multio/datamod/core/Record.h"

#include "multio/util/Print.h"


namespace multio::datamod {

// Compare functor == similar to `std::equal_to` that allows defining equal_to via this base classe
struct EqualToRecord {
    template <typename Record_, std::enable_if_t<IsRecord_v<Record_>, bool> = true>
    bool operator()(const Record_& lhs, const Record_& rhs) const {
        return std::apply(
            [&](const auto&... entryDef) {
                return ((entryDef.get(lhs) == entryDef.get(rhs)) && ... && true);
            },
            recordEntries<Record_>());
    };
};

// Compare functor != similar to `std::not_equal_to` that allows defining equal_to via this base classe
struct NotEqualToRecord {
    template <typename Record_, std::enable_if_t<IsRecord_v<Record_>, bool> = true>
    bool operator()(const Record_& lhs, const Record_& rhs) const {
        return !EqualToRecord{}(lhs, rhs);
    };
};

//-----------------------------------------------------------------------------

template <typename Rec, std::enable_if_t<IsRecord_v<Rec>, bool> = true>
bool operator==(const Rec& lhs, const Rec& rhs) {
    return EqualToRecord{}(lhs, rhs);
}

template <typename Rec, std::enable_if_t<IsRecord_v<Rec>, bool> = true>
bool operator!=(const Rec& lhs, const Rec& rhs) {
    return NotEqualToRecord{}(lhs, rhs);
}

}  // namespace multio::datamod
