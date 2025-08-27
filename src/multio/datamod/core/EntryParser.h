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

#include <type_traits>

#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Record.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------
// Reading from other containers
//-----------------------------------------------------------------------------

// Methods to parse single entries from arbitrary containers
// * `getByRef(BaseEntryDef, Container)`: Minimum to be defined
// information should be used to avoid too code generation for each field
// * `getByValue(BaseEntryDef, Container)`: Can be customized, but will use `getByRef` and perform `acquire`
// information should be used to avoid too code generation for each field
//
// BaseEntryDef is used to reduced compiled code - parser is not ment to use the accessor directly but ideal with all
// the string as accessor to containers
template <typename Container>
struct EntryParser {
    using UNSPECIALIZED_TAG = void;
};




// Default implementation for getByValue. Should be redefined by every implementation
template <
    typename Container,
    typename EntryDef_, typename Cont_,
    std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_> && std::is_base_of_v<Container, std::decay_t<Cont_>>), bool>
    = true>
EntryType_t<EntryDef_> getByValueThroughRef(const EntryDef_& entryDef, Cont_&& c) {
    using Ret = EntryType_t<EntryDef_>;
    Ret ret{EntryParser<Container>::getByRef(entryDef, std::forward<Cont_>(c))};
    ret.acquire();
    return ret;
}


//-----------------------------------------------------------------------------
// Implementation template
//-----------------------------------------------------------------------------

// template <typename MyContainer>
// struct EntryParser : BaseEntryParser<MyContainer> {
//     using Base = BaseEntryParser<MyContainer>;
//     using Base::getByValue;
//
//     template <
//         typename EntryDef_, typename Cont_,
//         std::enable_if_t<(IsEntryDefinition_v<EntryDef_> && std::is_base_of_v<MyContainer, std::decay_t<Cont_>>),
//         bool> = true>
//     static EntryType_t<EntryDef_> getByRef(const EntryDef_& entryDef, Cont_&& rec) {
//         return ...;
//     }
// };

template <typename Container, class = void>
struct EntryParserIsSpecialized : std::true_type {};

template <typename Container>
struct EntryParserIsSpecialized<Container, std::void_t<typename EntryParser<Container>::UNSPECIALIZED_TAG>>
    : std::false_type {};

template <typename Container>
inline constexpr bool EntryParserIsSpecialized_v = EntryParserIsSpecialized<Container>::value;

//-----------------------------------------------------------------------------
// Parsing record
//-----------------------------------------------------------------------------

template <typename EntryDef_, typename FromContainer,
          std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && !IsRecord_v<std::decay_t<FromContainer>>
                            && EntryParserIsSpecialized_v<std::decay_t<FromContainer>>),
                           bool>
          = true>
EntryType_t<EntryDef_> parseEntry(const EntryDef_& entryDef, FromContainer&& cont) {
    return EntryParser<std::decay_t<FromContainer>>::getByRef(entryDef.toBase(), std::forward<FromContainer>(cont));
}


template <typename EntryDef_, typename FromContainer,
          std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && !IsRecord_v<std::decay_t<FromContainer>>
                            && EntryParserIsSpecialized_v<std::decay_t<FromContainer>>),
                           bool>
          = true>
EntryType_t<EntryDef_> parseEntryByValue(const EntryDef_& entryDef, FromContainer&& cont) {
    return EntryParser<std::decay_t<FromContainer>>::getByValue(entryDef.toBase(), std::forward<FromContainer>(cont));
}


// parsing from other record
template <
    typename EntryDef_, typename FromContainer,
    std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && IsRecord_v<std::decay_t<FromContainer>>), bool>
    = true>
EntryType_t<EntryDef_> parseEntry(const EntryDef_& entryDef, FromContainer&& cont) {
    return entryDef.get(std::forward<FromContainer>(cont));
}


// parsing from other record
template <
    typename EntryDef_, typename FromContainer,
    std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && IsRecord_v<std::decay_t<FromContainer>>), bool>
    = true>
EntryType_t<EntryDef_> parseEntryByValue(const EntryDef_& entryDef, FromContainer&& cont) {
    auto ret = entryDef.get(std::forward<FromContainer>(cont));
    ret.acquire();
    return ret;
}


template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
void parseRecord(RecordType& rec, FromContainer&& cont) {
    std::apply(
        [&](const auto&... entryDef) {
            ((entryDef.get(rec) = parseEntry(entryDef, std::forward<FromContainer>(cont))), ...);
        },
        recordEntries(rec));
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
void parseRecordByValue(RecordType& rec, FromContainer&& cont) {
    std::apply(
        [&](const auto&... entryDef) {
            ((entryDef.get(rec) = parseEntryByValue(entryDef, std::forward<FromContainer>(cont))), ...);
        },
        recordEntries(rec));
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
RecordType parseRecord(FromContainer&& cont) {
    RecordType rec;
    parseRecord(rec, std::forward<FromContainer>(cont));
    return rec;
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
RecordType parseRecordByValue(FromContainer&& cont) {
    RecordType rec;
    parseRecordByValue(rec, std::forward<FromContainer>(cont));
    return rec;
}


//-----------------------------------------------------------------------------
// "Reading" record (parsing, applying defaults and validating)
//-----------------------------------------------------------------------------

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
void readRecord(RecordType& rec, FromContainer&& cont) {
    parseRecord(rec, std::forward<FromContainer>(cont));
    applyRecordDefaults(rec);
    validateRecord(rec);
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
RecordType readRecord(FromContainer&& cont) {
    RecordType rec;
    readRecord(rec, std::forward<FromContainer>(cont));
    return rec;
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
void readRecordByValue(RecordType& rec, FromContainer&& cont) {
    parseRecordByValue(rec, std::forward<FromContainer>(cont));
    applyRecordDefaults(rec);
    validateRecord(rec);
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
RecordType readRecordByValue(FromContainer&& cont) {
    RecordType rec;
    readRecordByValue(rec, std::forward<FromContainer>(cont));
    return rec;
}

}  // namespace multio::datamod

