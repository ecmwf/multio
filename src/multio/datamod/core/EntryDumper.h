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

#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Record.h"

namespace multio::datamod {

//-----------------------------------------------------------------------------
// Writing to other containers
//-----------------------------------------------------------------------------

// Methods to dump entries of a record to specific containers (i.e. to Metadata)
// set(EntryDef, Entry, Container)
template <typename Container>
struct EntryDumper {
    using UNSPECIALIZED_TAG = void;
};


//-----------------------------------------------------------------------------
// Implementation Template
//-----------------------------------------------------------------------------

// template <>
// struct EntryDumper<MyContainer> {
//     template <typename EntryDef_, typename Entry_, typename Cont_,
//               std::enable_if_t<(IsBaseEntryDefinition_v<std::decay_t<EntryDef_>> && IsEntry_v<std::decay_t<Entry_>>
//                                 && std::is_base_of_v<MyContainer, std::decay_t<Cont_>>),
//                                bool>
//               = true>
//     static void dump(const EntryDef_& entryDef, Entry_&& entry, Cont_& rec) {
//         // Implemenation
//     }
// };


template <typename Container, class = void>
struct EntryDumperIsSpecialized : std::true_type {};

template <typename Container>
struct EntryDumperIsSpecialized<Container, std::void_t<typename EntryDumper<Container>::UNSPECIALIZED_TAG>>
    : std::false_type {};

template <typename Container>
inline constexpr bool EntryDumperIsSpecialized_v = EntryDumperIsSpecialized<Container>::value;


//-----------------------------------------------------------------------------
// Dumping a record
//-----------------------------------------------------------------------------

template <
    typename EntryDef_, typename Entry_, typename Container,
    std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && IsEntry_v<std::decay_t<Entry_>>
                      && !IsRecord_v<std::decay_t<Container>> && EntryDumperIsSpecialized_v<std::decay_t<Container>>),
                     bool>
    = true>
void dumpEntry(const EntryDef_& entryDef, Entry_&& entry, Container& cont) {
    EntryDumper<std::decay_t<Container>>::set(entryDef.toBase(), std::forward<Entry_>(entry), cont);
}

// dump to other records
template <
    typename EntryDef_, typename Entry_, typename Container,
    std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && IsEntry_v<std::decay_t<Entry_>>
                      && IsRecord_v<std::decay_t<Container>> && EntryDumperIsSpecialized_v<std::decay_t<Container>>),
                     bool>
    = true>
void dumpEntry(const EntryDef_& entryDef, Entry_&& entry, Container& cont) {
    entryDef.get(cont).dump(std::forward<Entry_>(entry));
}


template <typename RecordType, typename Container,
          std::enable_if_t<EntryDumperIsSpecialized_v<std::decay_t<Container>>, bool> = true>
void dumpRecord(RecordType&& rec, Container& cont) {
    std::apply(
        [&](const auto&... entryDef) { (dumpEntry(entryDef, entryDef.get(std::forward<RecordType>(rec)), cont), ...); },
        recordEntries(rec));
}

template <typename Container, typename RecordType, std::enable_if_t<EntryDumperIsSpecialized_v<Container>, bool> = true>
Container dumpRecord(RecordType&& rec) {
    Container ret;
    dumpRecord(std::forward<RecordType>(rec), ret);
    return ret;
}


//-----------------------------------------------------------------------------

}  // namespace multio::datamod
