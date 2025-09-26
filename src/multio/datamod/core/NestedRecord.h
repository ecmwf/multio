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

#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"

/// Records can be nested - this is useful to express
/// nested metadata or YAML configuration. This is curretly used
/// to represent the MULTIOM encoder configuration - which has a lot of sub configurations.
/// Example:
///
/// ```
/// { "key1": "value1",
/// , "nested-rec":
///     { "nested-key1": "value"
///     , "nested-key2": "value"
///     }
/// }
/// ```
///
/// In this case "nested-rec" is derived from `RecordName_v<MyNestedRecord>`, where `MyNestedRecord`
/// contains 2 entries with "nested-key1" and "nested-key2".
///

namespace multio::datamod {

template <typename RecordType>
struct RecordMapper {
    // Parse record from other record or implemented container
    template <
        typename OtherRec,
        std::enable_if_t<IsRecord_v<std::decay_t<OtherRec>> || EntryParserIsSpecialized_v<std::decay_t<OtherRec>>, bool>
        = true>
    static RecordType parse(OtherRec&& rec) {
        return readRecordByValue<RecordType>(std::forward<OtherRec>(rec));
    }

    // Dump to other record
    template <
        typename Container, typename Val,
        std::enable_if_t<EntryDumperIsSpecialized_v<Container> && std::is_same_v<std::decay_t<Val>, RecordType>, bool>
        = true>
    static Container dumpTo(Val&& v) {
        return dumpRecord<Container>(std::forward<Val>(v));
    }
};


template <typename RecordType>
using NestedEntry_t = Entry<RecordType, RecordMapper<RecordType>>;

template <typename T, typename M>
constexpr auto nestedEntryDef(M T::*member) {
    static_assert(IsEntry_v<M>);
    using RecordType = typename M::ValueType;
    return entryDef(RecordName_v<RecordType>, member).withDefault([]() {
        RecordType rec;
        applyRecordDefaults(rec);
        return rec;
    });
}


template <typename T, typename M>
constexpr auto nestedOptEntryDef(M T::*member) {
    static_assert(IsEntry_v<M>);
    using RecordType = typename M::ValueType;
    return entryDef(RecordName_v<RecordType>, member).tagOptional();
}


//-----------------------------------------------------------------------------

}  // namespace multio::datamod

