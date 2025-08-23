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

#include <string_view>
#include <tuple>
#include <type_traits>
#include "multio/datamod/core/DataModellingException.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/util/TypeTraits.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------
// Definitions to handle records:
//  * Get a constexpr name
//  * Get a entry list as tuple
//-----------------------------------------------------------------------------


// Retrieves the name for a record - default is ta access a static member `record_name_`
template <typename RecordType>
struct RecordName {
    inline static constexpr std::string_view name = RecordType::record_name_;
};


// NOTE: Likely to be removed - introduced and used because of metadata ... but this seems to be an mistake 
template <typename RecordType>
struct ScopedRecord;


template <typename RecordType>
struct RecordName<ScopedRecord<RecordType>> {
    inline static constexpr std::string_view name = RecordType::record_name_;
};

// Get name of a keyset
template <typename RecordType>
inline constexpr std::string_view RecordName_v = RecordName<RecordType>::name;


//-----------------------------------------------------------------------------


template <typename RecordType, class = void>
struct HasRecordEntriesMember : std::false_type {};

template <typename RecordType>
struct HasRecordEntriesMember<RecordType, std::void_t<decltype(RecordType::record_entries_)>> : std::true_type {};

template <typename RecordType>
inline constexpr bool HasRecordEntriesMember_v = HasRecordEntriesMember<RecordType>::value;

//

template <typename RecordType, class = void>
struct HasScopedRecordEntriesMember : std::false_type {};

template <typename RecordType>
struct HasScopedRecordEntriesMember<RecordType, std::void_t<decltype(RecordType::scoped_record_entries_())>>
    : std::true_type {};

template <typename RecordType>
inline constexpr bool HasScopedRecordEntriesMember_v = HasScopedRecordEntriesMember<RecordType>::value;


//

template <typename RecordType, std::enable_if_t<HasRecordEntriesMember_v<RecordType>, bool> = true>
constexpr const auto& recordEntries() {
    return RecordType::record_entries_;
};

template <typename RecordType,
          std::enable_if_t<HasRecordEntriesMember_v<RecordType> && !HasScopedRecordEntriesMember_v<RecordType>, bool>
          = true>
constexpr const auto& recordEntries(const RecordType&) {
    return RecordType::record_entries_;
};


template <typename RecordType,
          std::enable_if_t<HasRecordEntriesMember_v<RecordType> && HasScopedRecordEntriesMember_v<RecordType>, bool>
          = true>
const auto& recordEntries(const RecordType& rec) {
    if (rec.custom_scoped_record_entries_) {
        return *rec.custom_scoped_record_entries_;
    }
    return rec.scoped_record_entries_();
};


//-----------------------------------------------------------------------------

template <typename RecordType, class = void>
struct IsRecord : std::false_type {};

template <typename RecordType>
struct IsRecord<RecordType, std::void_t<decltype(recordEntries(std::declval<const RecordType&>()))>> : std::true_type {
};

template <typename RecordType>
inline constexpr bool IsRecord_v = IsRecord<RecordType>::value;


template <typename RecordType>
struct IsScopedRecord : std::false_type {};

template <typename RecordType>
struct IsScopedRecord<ScopedRecord<RecordType>> : std::true_type {};

template <typename RecordType>
inline constexpr bool IsScopedRecord_v = IsScopedRecord<RecordType>::value;


//-----------------------------------------------------------------------------

template <typename Entries>
auto makeScopedEntries(const Entries& entries, const std::string& scope) {
    return std::apply(
        [&](const auto&... entryDef) {
            return std::make_tuple(scopedEntryDef(entryDef, scope + std::string("-") + std::string(entryDef.key()))...);
        },
        entries);
}
template <typename RecordType>
auto makeScopedEntries(const std::string& scope) {
    return makeScopedEntries(recordEntries<RecordType>(), scope);
}
template <typename RecordType>
auto makeScopedEntries() {
    return makeScopedEntries<RecordType>(std::string(RecordName_v<RecordType>));
}

template <typename RecordType>
using ScopedEntries_t = decltype(makeScopedEntries<RecordType>());

template <typename RecordType_>
struct ScopedRecord : RecordType_ {

    using RecordType = RecordType_;
    static const ScopedEntries_t<RecordType_>& scoped_record_entries_() {
        static const ScopedEntries_t<RecordType_> entries_{makeScopedEntries<RecordType_>()};
        return entries_;
    };
    std::optional<std::string> custom_scope_{};
    std::optional<ScopedEntries_t<RecordType_>> custom_scoped_record_entries_{};
};


template <typename RecordType, std::enable_if_t<IsRecord_v<std::decay_t<RecordType>>, bool> = true>
auto scopeRecord(RecordType&& rec) {
    return ScopedRecord<std::decay_t<RecordType>>{std::forward<RecordType>(rec)};
}

template <typename RecordType, std::enable_if_t<IsRecord_v<std::decay_t<RecordType>>, bool> = true>
auto scopeRecord(RecordType&& rec, const std::string& scope) {
    auto ret = ScopedRecord<std::decay_t<RecordType>>{std::forward<RecordType>(rec)};
    ret.custom_scoped_record_entries_ = makeScopedEntries(recordEntries(rec), scope);
    ret.custom_scope_ = std::move(scope);
    return ret;
}

template <typename SRecordType, std::enable_if_t<IsScopedRecord_v<std::decay_t<SRecordType>>, bool> = true>
typename std::decay_t<SRecordType>::RecordType unscopeRecord(SRecordType&& rec) {
    return std::forward<SRecordType>(rec);
}


template <typename SRecordType, std::enable_if_t<IsScopedRecord_v<std::decay_t<SRecordType>>, bool> = true>
std::string_view getRecordScope(SRecordType&& rec) {
    if (rec.custom_scope_) {
        return std::string_view(*rec.custom_scope_);
    }
    return RecordName_v<std::decay_t<SRecordType>>;
}


//-----------------------------------------------------------------------------

// Takes a string and searches the entry with the given key in the record.
// Then calls the passed function with the entry definition `func(entryDef)`.
template <typename Rec, typename Func, std::enable_if_t<IsRecord_v<std::decay_t<Rec>>, bool> = true>
void dispatchEntry(std::string_view key, const Rec& rec, Func&& func) {
    util::forEach(
        [&](const auto& entryDef) {
            if (entryDef.key() == key) {
                std::forward<Func>(func)(entryDef);
            }
            else {
                std::ostringstream oss;
                oss << "Can not dispatch key " << key << " on record " << RecordName_v<std::decay_t<Rec>>;
                throw DataModellingException(oss.str(), Here());
            }
        },
        recordEntries(rec));
}

//-----------------------------------------------------------------------------

template <typename... Records>
struct ComposedRecord : Records... {
    static constexpr auto record_entries_ = std::tuple_cat(recordEntries<Records>()...);

    static constexpr auto composing_records_ = std::make_tuple(util::TypeTag<Records>{}...);
};

template <typename RecordType, class = void>
struct HasComposingRecords : std::false_type {};

template <typename RecordType>
struct HasComposingRecords<RecordType, std::void_t<decltype(RecordType::composing_records_)>> : std::true_type {};

template <typename RecordType>
inline constexpr bool HasComposingRecords_v = HasComposingRecords<RecordType>::value;


template <typename RecordType, std::enable_if_t<HasComposingRecords_v<RecordType>, bool> = true>
constexpr const auto& composingRecords() {
    return RecordType::composing_records_;
};

template <typename RecordType, std::enable_if_t<HasComposingRecords_v<RecordType>, bool> = true>
constexpr const auto& composingRecords(const RecordType&) {
    return RecordType::composing_records_;
};

template <typename RecordType, std::enable_if_t<!HasComposingRecords_v<RecordType>, bool> = true>
constexpr auto composingRecords() {
    return std::tuple<>{};
};

template <typename RecordType, std::enable_if_t<!HasComposingRecords_v<RecordType>, bool> = true>
constexpr auto composingRecords(const RecordType&) {
    return std::tuple<>{};
};

//-----------------------------------------------------------------------------

// To be specialized by RecordType to provide custom default setting function
//
// Example
// ```
// template <>
// struct ApplyRecordDefaults<RecordType> {
//    static void applyDefaults(RecordType& record) {
//       ... custom operators like setting dependent default values
//           or performing complex consistency checks
//    }
// };
// ```
template <typename RecordType>
struct ApplyRecordDefaults;


template <typename RecordType, class = void>
struct HasApplyDefaults : std::false_type {};

template <typename RecordType>
struct HasApplyDefaults<
    RecordType, std::void_t<decltype(ApplyRecordDefaults<RecordType>::applyDefaults(std::declval<RecordType&>()))>>
    : std::true_type {};


template <typename RecordType>
inline constexpr bool HasApplyDefaults_v = HasApplyDefaults<RecordType>::value;


struct ApplyDefaultsFunctor {
    template <typename RecordType, std::enable_if_t<(HasApplyDefaults_v<RecordType>), bool> = true>
    void operator()(RecordType& rec) const {
        ApplyRecordDefaults<RecordType>::applyDefaults(rec);
    }

    template <typename RecordType, std::enable_if_t<(!HasApplyDefaults_v<RecordType>), bool> = true>
    void operator()(RecordType&) const {}
};

template <typename RecordType, std::enable_if_t<IsRecord_v<RecordType>, bool> = true>
void applyRecordDefaults(RecordType& rec) {
    // Iterate entries and set defaults
    std::apply([&](const auto&... entryDef) { (entryDef.applyDefaults(entryDef.get(rec)), ...); }, recordEntries(rec));

    std::apply([&](const auto&... tt) { (ApplyDefaultsFunctor{}(static_cast<util::Type_t<decltype(tt)>&>(rec)), ...); },
               composingRecords(rec));

    // Apply defaults on record
    ApplyDefaultsFunctor{}(rec);
}


//-----------------------------------------------------------------------------

// To be specialized by RecordType to provide additional validation
//
// Example
// ```
// template <>
// struct ValidateRecord<RecordType> {
//    static void validate(RecordType& record) {
//       ... custom operators like setting dependent default values
//           or performing complex consistency checks
//    }
// };
// ```
template <typename RecordType>
struct ValidateRecord;


template <typename RecordType, class = void>
struct HasValidate : std::false_type {};

template <typename RecordType>
struct HasValidate<RecordType,
                   std::void_t<decltype(ValidateRecord<RecordType>::validate(std::declval<const RecordType&>()))>>
    : std::true_type {};


template <typename RecordType>
inline constexpr bool HasValidate_v = HasValidate<RecordType>::value;


struct ValidateRecordFunctor {
    template <typename RecordType, std::enable_if_t<(HasValidate_v<RecordType>), bool> = true>
    void operator()(const RecordType& rec) const {
        ValidateRecord<RecordType>::validate(rec);
    }

    template <typename RecordType, std::enable_if_t<(!HasValidate_v<RecordType>), bool> = true>
    void operator()(RecordType&) const {}
};

template <typename RecordType, std::enable_if_t<IsRecord_v<RecordType>, bool> = true>
void validateRecord(const RecordType& rec) {
    // Iterate entries and set defaults
    std::apply([&](const auto&... entryDef) { (entryDef.validate(entryDef.get(rec)), ...); }, recordEntries(rec));

    std::apply(
        [&](const auto&... tt) { (ValidateRecordFunctor{}(static_cast<const util::Type_t<decltype(tt)>&>(rec)), ...); },
        composingRecords(rec));

    // Apply defaults on record
    ValidateRecordFunctor{}(rec);
}

//-----------------------------------------------------------------------------

template <typename RecordType, std::enable_if_t<IsRecord_v<RecordType>, bool> = true>
void acquireRecord(RecordType& rec) {
    // Iterate entries and set defaults
    std::apply([&](const auto&... entryDef) { (entryDef.get(rec).acquire(), ...); }, recordEntries(rec));

    // Apply defaults on record
    ValidateRecordFunctor{}(rec);
}

//-----------------------------------------------------------------------------

}  // namespace multio::datamod

