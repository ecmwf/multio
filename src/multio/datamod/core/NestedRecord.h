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


namespace multio::datamod {

template <typename RecordType>
struct RecordMapper {
    // Parse record from other record or implemented container
    template <
        typename OtherRec,
        std::enable_if_t<IsRecord_v<std::decay_t<OtherRec>> || EntryParser<std::decay_t<OtherRec>>::isSpecialized, bool>
        = true>
    static RecordType parse(OtherRec&& rec) {
        return readRecordByValue<RecordType>(std::forward<OtherRec>(rec));
    }

    // Dump to other record
    template <
        typename Container, typename Val,
        std::enable_if_t<EntryDumper<Container>::isSpecialized && std::is_same_v<std::decay_t<Val>, RecordType>, bool>
        = true>
    static Container dumpTo(Val&& v) {
        return dumpRecord<Container>(std::forward<Val>(v));
    }
};


template <typename RecordType>
constexpr auto nestedRecord() {
    return EntryDef<RecordType, RecordMapper<RecordType>>{RecordName_v<RecordType>}.withDefault([]() {
        RecordType rec;
        applyRecordDefaults(rec);
        return rec;
    });
}


template <typename RecordType>
constexpr auto nestedOptRecord() {
    return EntryDef<RecordType, RecordMapper<RecordType>>{RecordName_v<RecordType>}.tagOptional();
}


//-----------------------------------------------------------------------------

}  // namespace multio::datamod

