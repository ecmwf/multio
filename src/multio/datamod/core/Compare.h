/*
 * (C) Copyright 1996- ECMWF.
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


struct EqualToEntry {
    template <typename Entry_, std::enable_if_t<IsEntry_v<Entry_>, bool> = true>
    bool operator()(const Entry_& lhs, const Entry_& rhs) const {
        if (lhs.isUnset()) {
            return rhs.isUnset();
        }
        if (rhs.isUnset()) {
            return false;
        }
        return lhs.get() == rhs.get();
    };
};

struct NotEqualToEntry {
    template <typename Entry_, std::enable_if_t<IsEntry_v<Entry_>, bool> = true>
    bool operator()(const Entry_& lhs, const Entry_& rhs) const {
        return !EqualToEntry{}(lhs, rhs);
    };
};

struct EqualToRecord {
    template <typename Record_, std::enable_if_t<IsRecord_v<Record_>, bool> = true>
    bool operator()(const Record_& lhs, const Record_& rhs) const {
        return std::apply(
            [&](const auto&... entryDef) {
                return ((entryDef.get(lhs) == entryDef.get(rhs)) && ... && true);
                ;
            },
            recordEntries<Record_>());
    };
};

struct NotEqualToRecord {
    template <typename Record_, std::enable_if_t<IsRecord_v<Record_>, bool> = true>
    bool operator()(const Record_& lhs, const Record_& rhs) const {
        return !EqualToRecord{}(lhs, rhs);
    };
};

//-----------------------------------------------------------------------------

template <typename Entry_, std::enable_if_t<IsEntry_v<Entry_>, bool> = true>
bool operator==(const Entry_& lhs, const Entry_& rhs) {
    return EqualToEntry{}(lhs, rhs);
};

template <typename Entry_, std::enable_if_t<IsEntry_v<Entry_>, bool> = true>
bool operator!=(const Entry_& lhs, const Entry_& rhs) {
    return !EqualToEntry{}(lhs, rhs);
}


template <typename Rec, std::enable_if_t<IsRecord_v<Rec>, bool> = true>
bool operator==(const Rec& lhs, const Rec& rhs) {
    return EqualToRecord{}(lhs, rhs);
}

template <typename Rec, std::enable_if_t<IsRecord_v<Rec>, bool> = true>
bool operator!=(const Rec& lhs, const Rec& rhs) {
    return NotEqualToRecord{}(lhs, rhs);
}

}  // namespace multio::datamod
