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

struct ParseOptions {
    bool allowAdditionalKeys = true;
};

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
    typename Container, typename EntryDef_, typename Cont_,
    std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_> && std::is_base_of_v<Container, std::decay_t<Cont_>>), bool>
    = true>
EntryType_t<EntryDef_> getByValueThroughRef(const EntryDef_& entryDef, Cont_&& c,
                                            const ParseOptions& opts = ParseOptions{}) {
    using Ret = EntryType_t<EntryDef_>;
    Ret ret{EntryParser<Container>::getByRef(entryDef, std::forward<Cont_>(c), opts)};
    ret.acquire();
    return ret;
}


template <typename T, typename Rec, class = void>
struct HasCheckForNoAdditionalKeys : std::false_type {};

template <typename T, typename Rec>
struct HasCheckForNoAdditionalKeys<T, Rec,
                                   std::void_t<decltype(EntryParser<T>::checkForNoAdditionalKeys(
                                       std::declval<const T&>(), std::declval<const Rec&>()))>> : std::true_type {};

template <typename T, typename Rec>
inline constexpr bool HasCheckForNoAdditionalKeys_v = HasCheckForNoAdditionalKeys<T, Rec>::value;

// template <typename T, typename Rec>
// requires RecordType<Rec>
// concept HasCheckForNoAdditionalKeys = requires(const T& container, const Rec& rec) {
//     { EntryParser<T>::checkKeys(container, rec) } -> std::same_as<void>;
// };

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
//     static EntryType_t<EntryDef_> getByRef(const EntryDef_& entryDef, Cont_&& rec, const ParseOptions& opts =
//     ParseOptions{}) {
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

/// C++20 concept
// template <typename Container>
// concept HasUnspecializedTag = requires {
//     typename EntryParser<Container>::UNSPECIALIZED_TAG;
// };

// template <typename Container>
// concept EntryParserIsSpecialized = !HasUnspecializedTag<Container>;

template <typename T, typename Rec,
          std::enable_if_t<HasCheckForNoAdditionalKeys_v<std::decay_t<T>, std::decay_t<Rec>>, bool> = true>
void checkForNoAdditionalKeys(const T& container, const Rec& record, const ParseOptions& opts) {
    if (!opts.allowAdditionalKeys) {
        EntryParser<T>::checkForNoAdditionalKeys(container, record);
    }
}

template <typename T, typename Rec,
          std::enable_if_t<!HasCheckForNoAdditionalKeys_v<std::decay_t<T>, std::decay_t<Rec>>, bool> = true>
void checkForNoAdditionalKeys(const T& container, const Rec& record, const ParseOptions& opts) {
    // TODO(pgeier) Throw or warn if allowAdditionalKeys is false
}

//-----------------------------------------------------------------------------
// Parsing record
//-----------------------------------------------------------------------------

template <typename EntryDef_, typename FromContainer,
          std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && !IsRecord_v<std::decay_t<FromContainer>>
                            && EntryParserIsSpecialized_v<std::decay_t<FromContainer>>),
                           bool>
          = true>
EntryType_t<EntryDef_> parseEntry(const EntryDef_& entryDef, FromContainer&& cont,
                                  const ParseOptions& opts = ParseOptions{}) {
    return EntryParser<std::decay_t<FromContainer>>::getByRef(entryDef.toBase(), std::forward<FromContainer>(cont),
                                                              opts);
}


template <typename EntryDef_, typename FromContainer,
          std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && !IsRecord_v<std::decay_t<FromContainer>>
                            && EntryParserIsSpecialized_v<std::decay_t<FromContainer>>),
                           bool>
          = true>
EntryType_t<EntryDef_> parseEntryByValue(const EntryDef_& entryDef, FromContainer&& cont,
                                         const ParseOptions& opts = ParseOptions{}) {
    return EntryParser<std::decay_t<FromContainer>>::getByValue(entryDef.toBase(), std::forward<FromContainer>(cont),
                                                                opts);
}


// parsing from other record
template <
    typename EntryDef_, typename FromContainer,
    std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && IsRecord_v<std::decay_t<FromContainer>>), bool>
    = true>
EntryType_t<EntryDef_> parseEntry(const EntryDef_& entryDef, FromContainer&& cont,
                                  const ParseOptions& = ParseOptions{}) {
    return entryDef.get(std::forward<FromContainer>(cont));
}


// parsing from other record
template <
    typename EntryDef_, typename FromContainer,
    std::enable_if_t<(IsEntryDefinition_v<std::decay_t<EntryDef_>> && IsRecord_v<std::decay_t<FromContainer>>), bool>
    = true>
EntryType_t<EntryDef_> parseEntryByValue(const EntryDef_& entryDef, FromContainer&& cont,
                                         const ParseOptions& opts = ParseOptions{}) {
    auto ret = entryDef.get(std::forward<FromContainer>(cont));
    ret.acquire();
    return ret;
}


template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
void parseRecord(RecordType& rec, FromContainer&& cont, const ParseOptions& opts = ParseOptions{}) {
    checkForNoAdditionalKeys(cont, rec, opts);
    std::apply(
        [&](const auto&... entryDef) {
            ((entryDef.get(rec) = parseEntry(entryDef, std::forward<FromContainer>(cont), opts)), ...);
        },
        recordEntries(rec));
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
void parseRecordByValue(RecordType& rec, FromContainer&& cont, const ParseOptions& opts = ParseOptions{}) {
    checkForNoAdditionalKeys(cont, rec, opts);
    std::apply(
        [&](const auto&... entryDef) {
            ((entryDef.get(rec) = parseEntryByValue(entryDef, std::forward<FromContainer>(cont), opts)), ...);
        },
        recordEntries(rec));
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
RecordType parseRecord(FromContainer&& cont, const ParseOptions& opts = ParseOptions{}) {
    RecordType rec;
    parseRecord(rec, std::forward<FromContainer>(cont), opts);
    return rec;
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
RecordType parseRecordByValue(FromContainer&& cont, const ParseOptions& opts = ParseOptions{}) {
    RecordType rec;
    parseRecordByValue(rec, std::forward<FromContainer>(cont), opts);
    return rec;
}


//-----------------------------------------------------------------------------
// "Reading" record (parsing, applying defaults and validating)
//-----------------------------------------------------------------------------

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
void readRecord(RecordType& rec, FromContainer&& cont, const ParseOptions& opts = ParseOptions{}) {
    parseRecord(rec, std::forward<FromContainer>(cont), opts);
    applyRecordDefaults(rec);
    validateRecord(rec);
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
RecordType readRecord(FromContainer&& cont, const ParseOptions& opts = ParseOptions{}) {
    RecordType rec;
    readRecord(rec, std::forward<FromContainer>(cont), opts);
    return rec;
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
void readRecordByValue(RecordType& rec, FromContainer&& cont, const ParseOptions& opts = ParseOptions{}) {
    parseRecordByValue(rec, std::forward<FromContainer>(cont), opts);
    applyRecordDefaults(rec);
    validateRecord(rec);
}

template <typename RecordType, typename FromContainer,
          std::enable_if_t<
              IsRecord_v<std::decay_t<FromContainer>> || EntryParserIsSpecialized_v<std::decay_t<FromContainer>>, bool>
          = true>
RecordType readRecordByValue(FromContainer&& cont, const ParseOptions& opts = ParseOptions{}) {
    RecordType rec;
    readRecordByValue(rec, std::forward<FromContainer>(cont), opts);
    return rec;
}

}  // namespace multio::datamod

