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


/// \defgroup datamod_core
/// \page datamod_about About
/// \ingroup datamod_core
///
/// For the purpose of datamodelling a (small) reflective-lite framework is used.
/// It allows describing key/single entities as `Entry` and combine them in a `struct` (a **Record**)
/// that allows compile-time iteration to generate parsers, dumpers, printing etc.
/// Moreover it expresses follow up mechanisms like `applyRecordDefaults` and `validateRecord`.
/// The mechanism is used to express *data models* like groups of mars keys, grib keys or action specific metadata
/// expectations.
/// C++ does not offer any reflective features (not yet), but has a powerful template system.
/// This framework tries to be minimal but yet provides mechanisms to frame common repeating mechanisms.

/// \page datamod_motivation Motivation
/// \ingroup datamod_core
///
/// This page explains how to use this reflective-lite framework to model data descriptions.
/// The reason to have it and advantages of using it are the following:
///  - data models describe domain specific logic - all other technical components should be able to use it
///    and there must be clear separation between technical solutions/logic and the application domain.
///  - while is is completely legitimate to introduce duplication for technical mechanisms,
///    it is wise to avoid for domain knowledge.
///      - reduce risk of inconsistency
///      - simplify maintenance
///      - low cognitive overhead / try to have *one truth*
///  - domain logic is often driven by data schemas, i.e. objects that describe or are passed between different domain
///    specific operations. For ECMWF and this use case it are:
///      - sub groups of MARS keys
///      - sub groups of GRIB2 (low-level) or ECCODES (high-level) keys
///      - configuration of actions (e.g. input to MIR)
///      - custom data models for actions/expected metadata content e.g
///        - universal time description for temporal statistics to which different models can map to (including
///        different time axes for MARS)
///  - these descriptios are usually a set of keys with specific types. In C++ the choice of representation may be a
///  `struct`.
///    While for interfaces the shared object might be a valid `JSON` that can be described through a schema.
///    In python it might be described with `pydantic`.
///  - usual operations on these set of keys are:
///     - parsing to internal representation (C++ struct) from an external container (`JSON`,
///     `eckit::LocalConfiguration`, custom metadata object, GRIB handle)
///     - dumping from an internal representation (C++ struct) to an external container
///     - printing, comparing or even hashing the internal representation
///     - converting/mapping different representations for single keys:
///       - `param` may be represented as int and mapped from strings
///       - enum types are converted from/to strings or integers
///       - a time duration, or specific date/time can be described by a specific string format
///

/// \page datamod_usage Usage guide
///
/// # Defining entries
///
/// \code{.cpp}
/// #include "datamod/core/EntryDef.h"
/// namespace dm = multio::datamod;
///
/// // Define single keys as entry definitons
/// constexpr auto MyKey =
///     dm::EntryDef<std::string>{"my-key"}
///         .withDefault("a pleasant value")
///         .withAccessor([](auto&& v) { return &v.myKey; });
///
/// constexpr auto MyRequiredKey =
///     dm::EntryDef<std::string>{"my-required-key"}
///         .withAccessor([](auto&& v) { return &v.myRequiredKey; });
///
/// constexpr auto MyOptionalKey =
///     dm::EntryDef<std::string>{"my-optional-key"}
///         .tagOptional()
///         .withAccessor([](auto&& v) { return &v.myOptionalKey; });
/// \endcode
///
///
/// # Defining records
///
/// \code{.cpp}
/// #include "datamod/core/Record.h"
/// namespace dm = multio::datamod;
///
/// struct MyRecord {
///     // Define members with the same accessors as defined in the entry
///     dm::EntryType_t<decltype(MyKey)> myKey;
///     dm::EntryType_t<decltype(MyRequiredKey)> myRequiredKey;
///     dm::EntryType_t<decltype(MyOptionalKey)> myOptionalKey;
///
///     // Give the record a name (for printing or nesting...)
///     static constexpr std::string_view record_name_ = "my-record";
///
///     // List up all the entries in a compile time tuple. This make it a `record`.
///     static constexpr auto record_entries_ = std::make_tuple(MyKey, MyRequiredKey, MyOptionalKey);
///
///     // Optional - apply context dependent defaults
///     static void applyDefaults(RecordType& record) {
///        ... custom operators like setting dependent default values
///            or performing complex consistency checks
///     }
///
///     // Optional - perform context dependent validation
///     static void validate(const RecordType& record) {
///        ... do checks and potentially throw
///     }
/// };
/// \endcode
///
///
/// # Using entries
///
/// \code{.cpp}
/// #include "datamod/ContainerInterop.h"
/// namespace dm = multio::datamod;
///
/// multio::Metadata& md = message.metadata();
///
/// // Parse a single key to an entry -
/// // no defaults are applied, no validation happens
/// auto myKey = dm::parseEntry(MyKey, md);
///
/// // Work on the entry with
/// // `isSet()`, `set(...)`, `get(...)`
/// if (myKey.isSet()) {
///   std::cout << myKey.get() << std::endl;
/// }
///
/// // Writing an entry to an container
/// dm::dumpEntry(MyKey, myKey, md);
/// \endcode
///
///
/// # Using records
///
/// \code{.cpp}
/// #include "datamod/ContainerInterop.h"
/// namespace dm = multio::datamod;
///
/// multio::Metadata& md = message.metadata();
///
/// // Parsing a record, no defaults are applied, no valiadation
/// auto myRecord = dm::parseRecord<MyRecord>(md);
/// // Apply defaults
/// dm::applyRecordDefaults(myRecord);
/// // Validate - throws dm::DataModellingException for every key that is not set and not tagged optional.
/// dm::validateRecord(myRecord);
///
///
/// // Parsing, applying defaults & validating in one go
/// auto myRecord = dm::readRecord<MyRecord>(md);
///
/// // Or reading/parising to an existing entity
/// MyRecord myRecord;
/// dm::readRecord(myRecord, md);
///
/// // Using the record
///
/// myRecord.myKey.get();    // myKey has a default value - safe to use after applying defaults
/// myRecord.myReqKey.get(); // myReqKey is required - safe to use after validation
///
/// // Optional keys need checking
/// if (myRecord.myOptionalKey.isSet()) {
///   myRecord.myOptionalKey.get();
/// }
///
/// // Or set custom defaults
/// myRecord.myOptionalKey.ensureInit("eat something").get();
///
/// // Writing the record
/// dm::dumpRecord(myRecord, md);
///
/// \endcode


/// \defgroup datamod_core_records Records
/// \ingroup datamod_core
///
/// # Records
/// ## TO BE DONE
///
/// ```
/// TBD
/// ```
///


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

template <typename Rec, class = void>
struct IsRecord : std::false_type {};

template <typename Rec>
struct IsRecord<Rec, std::void_t<decltype(recordEntries(std::declval<const Rec&>()))>> : std::true_type {};

template <typename Rec>
inline constexpr bool IsRecord_v = IsRecord<Rec>::value;

/// C++20 concept
// template <typename T>
// concept RecordType = IsRecord<std::remove_cvref_t<T>>::value;


template <typename Rec>
struct IsScopedRecord : std::false_type {};

template <typename Rec>
struct IsScopedRecord<ScopedRecord<Rec>> : std::true_type {};

template <typename Rec>
inline constexpr bool IsScopedRecord_v = IsScopedRecord<Rec>::value;

/// C++20 concept
// template <typename T>
// concept ScopedRecordType = IsScopedRecord<std::remove_cvref_t<T>>::value;


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

template <typename Rec, std::enable_if_t<IsRecord_v<std::decay_t<Rec>>, bool> = true>
bool containsKey(std::string_view key, const Rec& rec) {
    return std::apply([&](const auto&... entryDef) { return ((entryDef.key() == key) || ... || false); }, recordEntries(rec));
}

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

/// \defgroup datamod_core_composedrecord ComposedRecord
/// \ingroup datamod_core_records
///
/// \section ComposedRecord
///
/// \brief Flat composing of records
///
/// \details `ComposedRecord<...>` allows using inheritance to combine and flatten multiple records.
///   This is all working through C++ inheritance mechanism.
///   The additional wrapping in `ComposedRecord<...>` ensures that all the `record_entries_`
///   are combined properly.
///
/// \cond
template <typename... Records>
struct ComposedRecord : Records... {
    static constexpr auto record_entries_ = std::tuple_cat(recordEntries<Records>()...);

    static constexpr auto composing_records_ = std::make_tuple(util::TypeTag<Records>{}...);
};
/// \endcond

/// \defgroup datamod_core_hascomposedrecords HasComposedRecords_v
/// \ingroup datamod_core_records
/// \brief True for types deriving from ComposedRecord
/// \details SFINAE predicate to check if a record has composing records. `false`
///          for any other types.
/// \cond
template <typename RecordType, class = void>
struct HasComposingRecords : std::false_type {};

template <typename RecordType>
struct HasComposingRecords<RecordType, std::void_t<decltype(RecordType::composing_records_)>> : std::true_type {};

template <typename RecordType>
inline constexpr bool HasComposingRecords_v = HasComposingRecords<RecordType>::value;


/// C++20 concept
// template <typename T>
// concept ComposedRecordType = HasComposedRecords<std::remove_cvref_t<T>>::value;

/// \endcond


/// \defgroup datamod_core_composingrecords composingRecords
/// \ingroup datamod_core_records
/// \brief    Compile-time function to return a tuple with record types.
/// \details  The tuple is not ment to be instantiated.
/// \cond
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
/// \endcond

//-----------------------------------------------------------------------------


template <typename RecordType, class = void>
struct HasApplyDefaults : std::false_type {};

template <typename RecordType>
struct HasApplyDefaults<RecordType, std::void_t<decltype(RecordType::applyDefaults(std::declval<RecordType&>()))>>
    : std::true_type {};


template <typename RecordType>
inline constexpr bool HasApplyDefaults_v = HasApplyDefaults<RecordType>::value;


struct ApplyDefaultsFunctor {
    template <typename RecordType, std::enable_if_t<(HasApplyDefaults_v<RecordType>), bool> = true>
    void operator()(RecordType& rec) const {
        RecordType::applyDefaults(rec);
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


template <typename RecordType, class = void>
struct HasValidate : std::false_type {};

template <typename RecordType>
struct HasValidate<RecordType, std::void_t<decltype(RecordType::validate(std::declval<const RecordType&>()))>>
    : std::true_type {};


template <typename RecordType>
inline constexpr bool HasValidate_v = HasValidate<RecordType>::value;


struct ValidateRecordFunctor {
    template <typename RecordType, std::enable_if_t<(HasValidate_v<RecordType>), bool> = true>
    void operator()(const RecordType& rec) const {
        RecordType::validate(rec);
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

