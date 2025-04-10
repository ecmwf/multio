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

/// @date Sep 2023

#pragma once

#include <tuple>
#include <type_traits>
#include "multio/message/Metadata.h"
#include "multio/util/Hash.h"
#include "multio/util/TypeTraits.h"


namespace multio::message {


//-----------------------------------------------------------------------------
// Definitions to handle key sets
//-----------------------------------------------------------------------------

/* To be specialized to stringify a enum as namespace
 */
template <typename EnumType>
struct KeySet;

template <typename EnumType>
inline constexpr std::string_view KeySetName_v = KeySet<EnumType>::name;


template <typename EnumType>
constexpr const auto& keySet() {
    static_assert(util::IsTuple_v<std::decay_t<decltype(KeySet<EnumType>::keys())>>, "Expected a tuple");
    return KeySet<EnumType>::keys();
}


namespace keyUtils {
template <auto val, typename T1, typename... T, std::enable_if_t<(sizeof...(T) == 0), bool> = true>
constexpr std::size_t getKeyIndexById() {
    static_assert(std::is_same_v<std::decay_t<decltype(T1::id)>, decltype(val)>);
    static_assert(T1::id == val, "Non of the types match the key");
    return 0;
}
template <auto val, typename T1, typename... T, std::enable_if_t<(sizeof...(T) > 0), bool> = true>
constexpr std::size_t getKeyIndexById() {
    if constexpr (std::is_same_v<std::decay_t<decltype(T1::id)>, decltype(val)> && (T1::id == val)) {
        return 0;
    }
    else {
        return 1 + getKeyIndexById<val, T...>();
    }
    return 0;  // Unreachable - avoid compiler warning
}


template <auto keyId, typename... T>
const auto& getById(const std::tuple<T...>& tup) {
    return std::get<getKeyIndexById<keyId, T...>()>(tup);
}


template <typename Func, typename Tup, std::size_t... I,
          std::enable_if_t<util::IsTuple_v<std::decay_t<Tup>>, bool> = true>
void withKeySet(Func&& func, Tup&& tup, std::index_sequence<I...>) {
    // avoid std::apply to reduce compile-time complexity
    (func(std::get<I>(std::forward<Tup>(tup))), ...);
}
template <typename Func, typename Tup, std::enable_if_t<util::IsTuple_v<std::decay_t<Tup>>, bool> = true>
void withKeySet(Func&& func, Tup&& tup) {
    withKeySet(std::forward<Func>(func), std::forward<Tup>(tup),
               std::make_index_sequence<std::tuple_size_v<std::decay_t<Tup>>>());
}
}  // namespace keyUtils


template <auto keyId, typename... T>
const auto& key(const std::tuple<T...>& keySet) {
    using EnumType = decltype(keyId);
    return keyUtils::getById<keyId>(keySet);
}
template <auto keyId>
const auto& key() {
    return key<keyId>(keySet<decltype(keyId)>());
}


template <typename Func, typename Tup, std::enable_if_t<util::IsTuple_v<std::decay_t<Tup>>, bool> = true>
void withKeySet(Func&& func, Tup&& tup) {
    keyUtils::withKeySet(std::forward<Func>(func), std::forward<Tup>(tup));
}
template <typename KeySet_, typename Func>
void withKeySet(Func&& func) {
    withKeySet(std::forward<Func>(func), keySet<KeySet_>());
}


//-----------------------------------------------------------------------------
// Definitions to describe key-value pairs
//-----------------------------------------------------------------------------

// TODO describe required/optional in description

namespace description {
enum class Tags : std::uint64_t
{
    Required,
    Optional,
};
;
}  // namespace description


template <auto id, typename ValueType, typename Mapper = void>
struct KeyValueDescription;


template <typename T>
struct IsKeyValueDescription {
    static constexpr bool value = false;
};
template <auto id, typename ValueType, typename Mapper>
struct IsKeyValueDescription<KeyValueDescription<id, ValueType, Mapper>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeyValueDescription_v = IsKeyValueDescription<T>::value;


template <auto id_, typename ValueType_>
struct KeyValueDescription<id_, ValueType_, void> {
    using KeyType = typename MetadataTypes::KeyType;
    using ValueType = ValueType_;
    using Mapper = void;

    KeyType key;

    static constexpr auto id = id_;

    operator KeyType&() { return key; }
    operator const KeyType&() const { return key; }
    operator std::string&() { return key; }
    operator const std::string&() const { return key; }
};


template <auto id_, typename ValueType_, typename Mapper_>
struct KeyValueDescription {
    using KeyType = typename MetadataTypes::KeyType;
    using ValueType = ValueType_;
    using Mapper = Mapper_;

    KeyType key;
    Mapper mapper;

    static const auto id = id_;

    operator KeyType&() { return key; }
    operator const KeyType&() const { return key; }
    operator std::string&() { return key; }
    operator const std::string&() const { return key; }
};


// Getter and setter functions to operate on some types with KeyValueDescription
// Can/Should be specilized by other types if needed

template <typename KVD, typename MD,
          std::enable_if_t<(IsKeyValueDescription_v<KVD> && std::is_same_v<void, typename KVD::Mapper>
                            && std::is_base_of_v<BaseMetadata, std::decay_t<MD>>),
                           bool>
          = true>
decltype(auto) get(const KVD& kvd, MD&& md) {
    return std::forward<MD>(md).template get<typename KVD::ValueType>(kvd.key);
    ;
}
template <typename KVD, typename MD,
          std::enable_if_t<(IsKeyValueDescription_v<KVD> && !std::is_same_v<void, typename KVD::Mapper>
                            && std::is_base_of_v<BaseMetadata, std::decay_t<MD>>),
                           bool>
          = true>
decltype(auto) get(const KVD& kvd, MD&& md) {
    return std::forward<MD>(md).get(kvd.key).visit(kvd.mapper);
}

template <typename KVD, typename MD,
          std::enable_if_t<(IsKeyValueDescription_v<KVD> && std::is_same_v<void, typename KVD::Mapper>
                            && std::is_base_of_v<BaseMetadata, std::decay_t<MD>>),
                           bool>
          = true>
decltype(auto) getOpt(const KVD& kvd, MD&& md) {
    return std::forward<MD>(md).template getOpt<typename KVD::ValueType>(kvd.key);
    ;
}
template <typename KVD, typename MD,
          std::enable_if_t<(IsKeyValueDescription_v<KVD> && !std::is_same_v<void, typename KVD::Mapper>
                            && std::is_base_of_v<BaseMetadata, std::decay_t<MD>>),
                           bool>
          = true>
decltype(auto) getOpt(const KVD& kvd, MD&& md) {
    if (auto search = std::forward<MD>(md).find(key); search != md.end()) {
        return search->second.visit(mapper);
    }
    return std::optional<typename KVD::ValueType> {}
}


template <typename KVD, typename MDV,
          std::enable_if_t<(IsKeyValueDescription_v<KVD> && std::is_same_v<void, typename KVD::Mapper>
                            && std::is_base_of_v<MetadataValue, std::decay_t<MDV>>),
                           bool>
          = true>
decltype(auto) get(const KVD& kvd, MDV&& md) {
    return std::forward<MDV>(md).template get<typename KVD::ValueType>();
    ;
}
template <typename KVD, typename MDV,
          std::enable_if_t<(IsKeyValueDescription_v<KVD> && !std::is_same_v<void, typename KVD::Mapper>
                            && std::is_base_of_v<MetadataValue, std::decay_t<MDV>>),
                           bool>
          = true>
decltype(auto) get(const KVD& kvd, MDV&& md) {
    return std::forward<MDV>(md).visit(kvd.mapper);
}

template <typename KVD, typename MD, typename ValType,
          std::enable_if_t<(IsKeyValueDescription_v<KVD> && std::is_same_v<void, typename KVD::Mapper>
                            && std::is_base_of_v<BaseMetadata, std::decay_t<MD>>),
                           bool>
          = true>
decltype(auto) set(const KVD& kvd, MD&& md, ValType&& value) {
    return std::forward<MD>(md).set(kvd.key, std::forward<ValType>(val));
}
template <typename KVD, typename MD, typename ValType,
          std::enable_if_t<(IsKeyValueDescription_v<KVD> && !std::is_same_v<void, typename KVD::Mapper>
                            && std::is_base_of_v<BaseMetadata, std::decay_t<MD>>),
                           bool>
          = true>
decltype(auto) set(const KVD& kvd, MD&& md, ValType&& value) {
    return std::forward<MD>(md).set(kvd.key, kvd.mapper(std::forward<ValType>(val)));
}


template <auto id_, typename ValueType_, typename Mapper = void>
struct PrefixedDescription {
    using KeyType = typename MetadataTypes::KeyType;
    using ValueType = ValueType_;

    static const auto id = id_;

    template <typename... Args>
    PrefixedDescription(const std::string& key, Args&&... args) :
        plain{key, args...}, prefixed{std::string(KeySetName_v<decltype(id_)>) + std::string("-") + key, args...} {}

    using KV = KeyValueDescription<id_, ValueType, Mapper>;
    KV plain;
    KV prefixed;
};

template <auto id_, typename ValueType, typename KeyType>
PrefixedDescription<id_, ValueType> prefixedDescription(KeyType&& key) {
    return PrefixedDescription<id_, ValueType>{std::forward<KeyType>(key)};
}
template <auto id_, typename ValueType, typename KeyType, typename Mapper>
PrefixedDescription<id_, ValueType, std::decay_t<Mapper>> prefixedDescription(KeyType&& key, Mapper&& mapper) {
    return PrefixedDescription<id_, ValueType, std::decay_t<Mapper>>{std::forward<KeyType>(key),
                                                                     std::forward<Mapper>(mapper)};
}


//-----------------------------------------------------------------------------
// Value containers
//-----------------------------------------------------------------------------

struct MissingValue {};

template <auto id_, typename ValueType_, typename Mapper>
struct KeyValue {
    using KeyType = typename MetadataTypes::KeyType;
    using ValueType = ValueType_;
    using This = KeyValue<id_, ValueType, Mapper>;
    static const auto id = id_;

    using Description = KeyValueDescription<id_, ValueType, Mapper>;

    using RefType = std::reference_wrapper<const ValueType>;
    using Container = std::variant<MissingValue, ValueType, RefType>;


    Description description;
    Container value;

    bool isMissing() const { return std::holds_alternative<MissingValue>(value); }
    bool holdsReference() const { return std::holds_alternative<const ValueType>(value); }

    const ValueType& get() const {
        return std::visit(eckit::Overloaded{
                              [&](const ValueType& val) -> const ValueType& { return val },
                              [&](const RefType& val) -> const ValueType& { return val.get(); },
                              [&](MissingValue) {
                                  throw MetadataException(
                                      std::string("Value is missing for key ") + std::string(description), Here());
                              },
                          },
                          value);
    }
    operator const ValueType&() const { return get(); }


    void set(MissingValue val) noexcept { value = val; }
    void setMissing() noexcept { value = MissingValue{}; }
    void set(ValueType&& val) noexcept { value = std::move(val); }
    void set(const ValueType& val) { value = val; }

    This& operator=(MissingValue val) noexcept { value = val; }
    This& operator=(ValueType&& val) noexcept { value = std::move(val); }
    This& operator=(const ValueType& val) { value = val; }

    // Make sure no reference is hold and value is owned
    void acquire() {
        std::visit(eckit::Overloaded{
                       [&](const RefType& val) { this->value = val.get(); },
                       [&](auto) {},
                   },
                   value);
    }
};


template <typename T>
struct IsKeyValue {
    static constexpr bool value = false;
};
template <auto id, typename ValueType, typename Mapper>
struct IsKeyValue<KeyValue<id, ValueType, Mapper>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeyValue_v = IsKeyValue<T>::value;


template <auto id, typename ValType, typename Mapper, typename V,
          std::enable_if_t<!IsKeyValue_v<std::decay_t<V>>, bool> = true>
decltype(auto) toKeyValueRef(const KeyValueDescription<id, ValType, Mapper>& descr, V&& val) {
    using KV = KeyValue<id, ValueType, Mapper>;
    if constexpr (!std::is_lvalue_reference_v<V> || std::is_same_v<std::decay_t<V>, MissingValue>) {
        return KV{descr, std::move(val)};
    }
    else {
        return KV{descr, typename KV::RefType(val)};
    }
}
template <auto id, typename ValType, typename Mapper, typename V,
          std::enable_if_t<!IsKeyValue_v<std::decay_t<V>>, bool> = true>
decltype(auto) toKeyValue(const KeyValueDescription<id, ValType, Mapper>& descr, V&& val) {
    using KV = KeyValue<id, ValueType, Mapper>;
    if constexpr (!std::is_lvalue_reference_v<V> || std::is_same_v<std::decay_t<V>, MissingValue>) {
        return KV{descr, std::move(val)};
    }
    else {
        return KV{descr, val};
    }
}


//-----------------------------------------------------------------------------


namespace mapper {
struct ParamMapper {
    std::int64_t operator()(std::int64_t) const noexcept;
    std::int64_t operator()(const std::string&) const;
    template <typename T>
    std::int64_t operator()(T&& t) const {
        throw MetadataException("Param must be an int or string", Here());
    }
};
struct IntToBoolMapper {
    inline bool operator()(bool v) const noexcept { return v; };
    inline bool operator()(std::int64_t v) const { return v > 0; };
    template <typename T>
    bool operator()(T&& t) const {
        throw MetadataException("Value must be an int or string", Here());
    }
};
}  // namespace mapper


//-----------------------------------------------------------------------------
// Mars Keys
//-----------------------------------------------------------------------------


enum class MarsKeys : std::uint64_t
{
    EXPVER,
    STREAM,
    TYPE,
    CLASS,
    ORIGIN,
    ANOFFSET,
    PACKING,
    NUMBER,
    IDENT,
    INSTRUMENT,
    CHANNEL,
    CHEM,
    PARAM,
    MODEL,
    LEVTYPE,
    LEVELIST,
    DIRECTION,
    FREQUENCY,
    DATE,
    TIME,
    STEP,
    TIMEPROC,
    HDATE,
    GRID,
    TRUNCATION,
    REPRES,
};

template <>
struct KeySet<MarsKeys> {
    static constexpr std::string_view name = "mars";

    static const auto& keys() {
        static const auto keys
            = std::make_tuple(prefixedDescription<MarsKeys::EXPVER, std::string>("expver"),
                              prefixedDescription<MarsKeys::STREAM, std::string>("stream"),
                              prefixedDescription<MarsKeys::TYPE, std::string>("type"),
                              prefixedDescription<MarsKeys::CLASS, std::string>("class"),

                              prefixedDescription<MarsKeys::ORIGIN, std::string>("origin"),
                              prefixedDescription<MarsKeys::ANOFFSET, std::int64_t>("anoffset"),
                              prefixedDescription<MarsKeys::PACKING, std::string>("packing"),
                              prefixedDescription<MarsKeys::NUMBER, std::int64_t>("number"),
                              prefixedDescription<MarsKeys::IDENT, std::int64_t>("ident"),
                              prefixedDescription<MarsKeys::INSTRUMENT, std::int64_t>("instrument"),
                              prefixedDescription<MarsKeys::CHANNEL, std::int64_t>("channel"),
                              prefixedDescription<MarsKeys::CHEM, std::int64_t>("chem"),
                              prefixedDescription<MarsKeys::PARAM, std::int64_t>("param", mapper::ParamMapper{}),

                              prefixedDescription<MarsKeys::MODEL, std::string>("model"),
                              prefixedDescription<MarsKeys::LEVTYPE, std::string>("levtype"),

                              prefixedDescription<MarsKeys::LEVELIST, std::int64_t>("levelist"),
                              prefixedDescription<MarsKeys::DIRECTION, std::int64_t>("direction"),
                              prefixedDescription<MarsKeys::FREQUENCY, std::int64_t>("frequency"),
                              prefixedDescription<MarsKeys::DATE, std::int64_t>("date"),
                              prefixedDescription<MarsKeys::TIME, std::int64_t>("time"),
                              prefixedDescription<MarsKeys::STEP, std::int64_t>("step"),
                              prefixedDescription<MarsKeys::TIMEPROC, std::int64_t>("timeproc"),
                              prefixedDescription<MarsKeys::HDATE, std::int64_t>("hdate"),

                              prefixedDescription<MarsKeys::GRID, std::string>("grid"),
                              prefixedDescription<MarsKeys::TRUNCATION, std::string>("truncation"));
        return keys;
    }
};


//-----------------------------------------------------------------------------
// Parametrization keys
//-----------------------------------------------------------------------------


enum class MiscKeys : std::uint64_t
{
    TablesVersion,
    GeneratingProcessIdentifier,
    Typeofprocesseddata,
    EncodeStepZero,
    InitialStep,
    LengthOfTimeRange,
    LengthOfTimeStep,
    LengthOfTimeRangeInSeconds,
    LengthOfTimeStepInSeconds,
    ValuesScaleFactor,
    Pv,
    NumberOfMissingValues,
    ValueOfMissingValues,
    TypeOfEnsembleForecast,
    NumberOfForecastsInEnsemble,
    LengthOfTimeWindow,
    LengthOfTimeWindowInSeconds,
    BitsPerValue,
    PeriodMin,
    PeriodMax,
    WaveDirections,
    WaveFrequencies,
    SatelliteSeries,
    ScaleFactorOfCentralWavenumber,
    ScaledValueOfCentralWavenumber,

    // TBD - move to mars
    MethodNumber,
    SystemNumber
};

template <>
struct KeySet<MiscKeys> {
    static constexpr std::string_view name = "misc";

    static const auto& keys() {
        static const auto keys = std::make_tuple(
            prefixedDescription<MiscKeys::TablesVersion, std::int64_t>("tablesVersion"),
            prefixedDescription<MiscKeys::GeneratingProcessIdentifier, std::int64_t>("generatingProcessIdentifier"),
            prefixedDescription<MiscKeys::Typeofprocesseddata, std::int64_t>("typeofprocesseddata"),
            prefixedDescription<MiscKeys::EncodeStepZero, bool>("encodeStepZero", mapper::IntToBoolMapper{}),
            prefixedDescription<MiscKeys::InitialStep, std::int64_t>("initialStep"),
            prefixedDescription<MiscKeys::LengthOfTimeRange, std::int64_t>("lengthOfTimeRange"),
            prefixedDescription<MiscKeys::LengthOfTimeStep, std::int64_t>("lengthOfTimeStep"),
            prefixedDescription<MiscKeys::LengthOfTimeRangeInSeconds, std::int64_t>("lengthOfTimeRangeInSeconds"),
            prefixedDescription<MiscKeys::LengthOfTimeStepInSeconds, std::int64_t>("lengthOfTimeStepInSeconds"),
            prefixedDescription<MiscKeys::ValuesScaleFactor, double>("valuesScaleFactor"),
            prefixedDescription<MiscKeys::Pv, std::vector<double>>("pv"),
            prefixedDescription<MiscKeys::NumberOfMissingValues, std::int64_t>("numberOfMissingValues"),
            prefixedDescription<MiscKeys::ValueOfMissingValues, double>("valueOfMissingValues"),
            prefixedDescription<MiscKeys::TypeOfEnsembleForecast, std::int64_t>("typeOfEnsembleForecast"),
            prefixedDescription<MiscKeys::NumberOfForecastsInEnsemble, std::int64_t>("numberOfForecastsInEnsemble"),
            prefixedDescription<MiscKeys::LengthOfTimeWindow, std::int64_t>("lengthOfTimeWindow"),
            prefixedDescription<MiscKeys::LengthOfTimeWindowInSeconds, std::int64_t>("lengthOfTimeWindowInSeconds"),
            prefixedDescription<MiscKeys::BitsPerValue, std::int64_t>("bitsPerValue"),
            prefixedDescription<MiscKeys::PeriodMin, std::int64_t>("periodMin"),
            prefixedDescription<MiscKeys::PeriodMax, std::int64_t>("periodMax"),
            prefixedDescription<MiscKeys::WaveDirections, std::vector<double>>("waveDirections"),
            prefixedDescription<MiscKeys::WaveFrequencies, std::vector<double>>("waveFrequencies"),
            prefixedDescription<MiscKeys::SatelliteSeries, std::int64_t>("satelliteSeries"),
            prefixedDescription<MiscKeys::ScaleFactorOfCentralWavenumber, std::int64_t>(
                "scaleFactorOfCentralWavenumber"),
            prefixedDescription<MiscKeys::ScaledValueOfCentralWavenumber, std::int64_t>(
                "scaledValueOfCentralWavenumber"),

            // TBD - move to marse
            prefixedDescription<MiscKeys::MethodNumber, std::int64_t>("methodNumber"),
            prefixedDescription<MiscKeys::SystemNumber, std::int64_t>("systemNumber"));
        return keys;
    }
};


//-----------------------------------------------------------------------------
// Geometry keys - gg
//-----------------------------------------------------------------------------

enum class GeoGG : std::uint64_t
{
    TruncateDegrees,
    NumberOfPointsAlongAMeridian,
    NumberOfParallelsBetweenAPoleAndTheEquator,
    LatitudeOfFirstGridPointInDegrees,
    LongitudeOfFirstGridPointInDegrees,
    LatitudeOfLastGridPointInDegrees,
    LongitudeOfLastGridPointInDegrees,
    Pl
};

template <>
struct KeySet<GeoGG> {
    static constexpr std::string_view name = "geo-gg";

    static const auto& keys() {
        static const auto keys = std::make_tuple(
            prefixedDescription<GeoGG::TruncateDegrees, std::int64_t>("truncateDegrees"),
            prefixedDescription<GeoGG::NumberOfPointsAlongAMeridian, std::int64_t>("numberOfPointsAlongAMeridian"),
            prefixedDescription<GeoGG::NumberOfParallelsBetweenAPoleAndTheEquator, std::int64_t>(
                "numberOfParallelsBetweenAPoleAndTheEquator"),
            prefixedDescription<GeoGG::LatitudeOfFirstGridPointInDegrees, double>("latitudeOfFirstGridPointInDegrees"),
            prefixedDescription<GeoGG::LongitudeOfFirstGridPointInDegrees, double>(
                "longitudeOfFirstGridPointInDegrees"),
            prefixedDescription<GeoGG::LatitudeOfLastGridPointInDegrees, double>("latitudeOfLastGridPointInDegrees"),
            prefixedDescription<GeoGG::LongitudeOfLastGridPointInDegrees, double>("longitudeOfLastGridPointInDegrees"),
            prefixedDescription<GeoGG::Pl, std::vector<std::int64_t>>("pl"));
        return keys;
    }
};

//-----------------------------------------------------------------------------
// Geometry keys - sh
//-----------------------------------------------------------------------------

enum class GeoSH : std::uint64_t
{
    PentagonalResolutionParameterJ,
    PentagonalResolutionParameterK,
    PentagonalResolutionParameterM
};

template <>
struct KeySet<GeoSH> {
    static constexpr std::string_view name = "geo-sh";

    static const auto& keys() {
        static const auto keys = std::make_tuple(
            prefixedDescription<GeoSH::PentagonalResolutionParameterJ, std::int64_t>("pentagonalResolutionParameterJ"),
            prefixedDescription<GeoSH::PentagonalResolutionParameterK, std::int64_t>("pentagonalResolutionParameterK"),
            prefixedDescription<GeoSH::PentagonalResolutionParameterM, std::int64_t>("pentagonalResolutionParameterM"));
        return keys;
    }
};

//-----------------------------------------------------------------------------
// Geometry keys - ll
//-----------------------------------------------------------------------------

enum class GeoLL : std::uint64_t
{
};

template <>
struct KeySet<GeoLL> {
    static constexpr std::string_view name = "geo-ll";

    static const auto& keys() {
        static auto keys = std::make_tuple();
        return keys;
    }
};


// namespace Mtg2 {

//     std::tuple<Repres, std::string> represAndPrefixFromGridName(const std::string& gridName);

//     template<typename Func>
//     void withGeometryKeys(Repres repres, Func&& func) {
//         switch (repres) {
//             case Repres::GG: {
//                 withGGKeys(std::forward<Func>(func));
//                 return;
//             }
//             case Repres::LL: {
//                 withLLKeys(std::forward<Func>(func));
//                 return;
//             }
//             case Repres::SH: {
//                 withSHKeys(std::forward<Func>(func));
//                 return;
//             }
//         }
//     }
// };


//-----------------------------------------------------------------------------
// To be refactored / replaced
//-----------------------------------------------------------------------------

/**
 * TODO old glossary ... will be refactored
 *
 * This class is ment to keep track of different metadata keys used within the action provided through multio.
 * Reasons to have this:
 *  - Keep track of metadata keys that are used - with a proper IDE we can jump to all places a key is used
 *  - Just using strings at multiple places is error prone (typos can happen)
 *  - In the future also type information and specialized access operations should be added
 *  - We can do proper benchmark of metadata operations with typical keys. Moreover its easy to benchmark different key
 * (fixed strings, prehashed strings in case of hashmaps) and maptypes
 */
struct Glossary {
    using KeyType = typename MetadataTypes::KeyType;

    template <typename ValueType, typename Mapper = void>
    using KV = KeyValueDescription<0, ValueType, Mapper>;

    // General keys
    const KeyType name{"name"};
    const KeyType paramId{"paramId"};
    const KeyType param{"param"};
    const KeyType globalSize{"misc-globalSize"};
    const KeyType domain{"domain"};
    const KeyType date{"date"};
    const KeyType time{"time"};
    const KeyType precision{"misc-precision"};

    // Added missing 08/04/2025
    const KeyType shortName{"shortName"};
    const KeyType unpackedSubsetPrecision{"unpackedSubsetPrecision"};
    const KeyType representation{"representation"};
    const KeyType trigger{"trigger"};
    const KeyType gridded{"gridded"};

    // Nemo
    const KeyType nemoParam{"nemoParam"};
    const KeyType category{"category"};

    // Mars keys
    const KV<std::string> type{"type"};
    const KV<std::string> marsType{"marsType"};
    const KV<std::string> classKey{"class"};
    const KV<std::string> marsClass{"marsClass"};
    const KV<std::string> stream{"stream"};
    const KV<std::string> marsStream{"marsStream"};
    const KV<std::string> expver{"expver"};
    const KV<std::string> experimentVersionNumber{"experimentVersionNumber"};
    const KV<std::int64_t> levelist{"levelist"};
    const KV<std::string> levtype{"levtype"};
    const KV<std::string> levtypeWam{"levtype_wam"};
    const KV<std::string> dataset{"dataset"};
    const KV<std::string> resolution{"resolution"};
    const KV<std::string> activity{"activity"};
    const KV<std::string> experiment{"experiment"};
    const KV<std::string> generation{"generation"};
    const KV<std::string> model{"model"};
    const KV<std::string> realization{"realization"};
    const KV<std::int64_t> methodNumber{"methodNumber"};
    const KV<std::int64_t> systemNumber{"systemNumber"};
    const KV<std::int64_t> methodNumberKC{"method-number"};  // Kebap case
    const KV<std::int64_t> systemNumberKC{"system-number"};  // Kebap case

    // Eccodes specific
    const KV<std::string> gribEdition{"gribEdition"};
    const KV<std::int64_t> tablesVersion{"tablesVersion"};
    const KV<std::int64_t> localTablesVersion{"localTablesVersion"};
    const KV<bool> setLocalDefinition{"setLocalDefinition"};
    const KV<std::int64_t> grib2LocalSectionNumber{"grib2LocalSectionNumber"};
    const KV<std::int64_t> extraLocalSectionNumber{"extraLocalSectionNumber"};
    const KV<bool> deleteExtraLocalSection{"deleteExtraLocalSection"};
    const KV<std::int64_t> productDefinitionTemplateNumber{"productDefinitionTemplateNumber"};
    const KV<std::int64_t> productionStatusOfProcessedData{"productionStatusOfProcessedData"};

    // Eccodes concepts
    const KV<std::string> gridName{"gridName"};
    const KV<std::string> gridType{"gridType"};
    const KV<std::string> typeOfLevel{"typeOfLevel"};
    const KV<std::int64_t> localDefinitionNumber{"localDefinitionNumber"};

    // Additional eccodes keys
    const KV<std::string> setPackingType{"setPackingType"};
    const KV<std::int64_t> complexPacking{"complexPacking"};
    const KV<double> missingValue{"missingValue"};
    const KV<std::int64_t> bitsPerValue{"bitsPerValue"};
    const KV<bool> bitmapPresent{"bitmapPresent"};

    // Grib general
    const KV<std::int64_t> typeOfGeneratingProcess{"typeOfGeneratingProcess"};  // Analog to mars type
    const KV<std::int64_t> generatingProcessIdentifier{"generatingProcessIdentifier"};
    const KV<std::string> subCentre{"subCentre"};

    const KV<std::int64_t> perturbationNumber{"perturbationNumber"};
    const KV<std::int64_t> numberOfForecastsInEnsemble{"numberOfForecastsInEnsemble"};
    const KV<std::int64_t> ensembleMember{"ensembleMember"};
    const KV<std::int64_t> ensembleSize{"ensembleSize"};
    const KV<std::int64_t> ensembleMemberKC{"ensemble-member"};  // Kebap case
    const KV<std::int64_t> ensembleSizeKC{"ensemble-size"};      // Kebap case
    const KV<std::int64_t> offsetToEndOf4DvarWindow{"offsetToEndOf4DvarWindow"};
    const KV<std::int64_t> lengthOf4DvarWindow{"lengthOf4DvarWindow"};

    const KV<std::int64_t> anoffset{"anoffset"};
    const KV<std::int64_t> anlength{"anlength"};

    const KV<std::int64_t> componentIndex{"componentIndex"};
    const KV<std::int64_t> numberOfComponents{"numberOfComponents"};
    const KV<std::int64_t> modelErrorType{"modelErrorType"};
    const KV<std::int64_t> iterationNumber{"iterationNumber"};
    const KV<std::int64_t> totalNumberOfIterations{"totalNumberOfIterations"};


    // Eccodes grib reference date/time - direct setting (alternative to date & time)
    const KV<std::int64_t> year{"year"};
    const KV<std::int64_t> month{"month"};
    const KV<std::int64_t> day{"day"};
    const KV<std::int64_t> hour{"hour"};
    const KV<std::int64_t> minute{"minute"};
    const KV<std::int64_t> second{"second"};

    const KV<std::int64_t> forecastTime{"forecastTime"};

    // Eccodes analysis date/time - direct setting (alternative to dateOfAnalysis & timeOfAnalysis) -- ONLY VALID FOR A
    // SPECIFIC localDefinitionNumber
    const KV<std::int64_t> yearOfAnalysis{"yearOfAnalysis"};
    const KV<std::int64_t> monthOfAnalysis{"monthOfAnalysis"};
    const KV<std::int64_t> dayOfAnalysis{"dayOfAnalysis"};
    const KV<std::int64_t> hourOfAnalysis{"hourOfAnalysis"};
    const KV<std::int64_t> minuteOfAnalysis{"minuteOfAnalysis"};
    const KV<std::int64_t> secondOfAnalysis{"secondOfAnalysis"};

    // Eccodes grib2 stat
    const KV<std::int64_t> yearOfEndOfOverallTimeInterval{"yearOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> monthOfEndOfOverallTimeInterval{"monthOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> dayOfEndOfOverallTimeInterval{"dayOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> hourOfEndOfOverallTimeInterval{"hourOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> minuteOfEndOfOverallTimeInterval{"minuteOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> secondOfEndOfOverallTimeInterval{"secondOfEndOfOverallTimeInterval"};
    const KV<std::string> typeOfStatisticalProcessing{"typeOfStatisticalProcessing"};
    const KV<std::int64_t> lengthOfTimeRange{"lengthOfTimeRange"};
    const KV<std::int64_t> indicatorOfUnitForTimeIncrement{"indicatorOfUnitForTimeIncrement"};
    const KV<std::int64_t> timeIncrement{"timeIncrement"};

    // Eccodes grib2 grid
    const KV<std::string> unstructuredGridType{"unstructuredGridType"};
    const KV<std::string> unstructuredGridSubtype{"unstructuredGridSubtype"};
    const KV<std::string> uuidOfHGrid{"uuidOfHGrid"};

    // Eccodes grib horizontal + vertial
    const KV<std::int64_t> level{"level"};
    const KV<std::int64_t> scaledValueOfFirstFixedSurface{"scaledValueOfFirstFixedSurface"};
    const KV<std::int64_t> scaledValueOfSecondFixedSurface{"scaledValueOfSecondFixedSurface"};
    const KV<std::int64_t> scaleFactorOfFirstFixedSurface{"scaleFactorOfFirstFixedSurface"};
    const KV<std::int64_t> scaleFactorOfSecondFixedSurface{"scaleFactorOfSecondFixedSurface"};
    const KV<std::int64_t> typeOfFirstFixedSurface{"typeOfFirstFixedSurface"};
    const KV<std::int64_t> typeOfSecondFixedSurface{"typeOfSecondFixedSurface"};

    // Time model
    const KV<std::int64_t> startTime{"startTime"};
    const KV<std::int64_t> startDate{"startDate"};
    const KV<std::int64_t> previousTime{"previousTime"};
    const KV<std::int64_t> previousDate{"previousDate"};
    const KV<std::int64_t> currentTime{"currentTime"};
    const KV<std::int64_t> currentDate{"currentDate"};

    const KV<std::int64_t> sampleInterval{"sampleInterval"};
    const KV<std::int64_t> sampleIntervalInSeconds{"sampleIntervalInSeconds"};

    // legacy & conversion
    const KV<std::int64_t> timeStep{"timeStep"};
    const KV<std::int64_t> step{"step"};
    const KV<std::int64_t> stepUnits{"stepUnits"};
    const KV<std::string> stepRange{"stepRange"};
    const KV<std::int64_t> startStep{"startStep"};
    const KV<std::int64_t> endStep{"endStep"};
    const KV<std::int64_t> dataTime{"dataTime"};
    const KV<std::int64_t> dataDate{"dataDate"};
    const KV<std::int64_t> indicatorOfUnitForTimeRange{"indicatorOfUnitForTimeRange"};

    const KV<std::int64_t> dateOfAnalysis{"date-of-analysis"};
    const KV<std::int64_t> timeOfAnalysis{"time-of-analysis"};

    // Statistic
    const KV<std::string> operation{"operation"};
    const KV<std::int64_t> restartStep{"restart-step"};
    const KV<std::int64_t> stepFrequency{"step-frequency"};

    // Healpix
    const KV<std::int64_t> nside{"Nside"};
    const KV<std::string> orderingConvention{"orderingConvention"};

    // Spherical harmonics
    const KeyType sphericalHarmonics{"sphericalHarmonics"};
    const KV<std::int64_t> pentagonalResolutionParameterJ{"pentagonalResolutionParameterJ"};
    const KV<std::int64_t> pentagonalResolutionParameterK{"pentagonalResolutionParameterK"};
    const KV<std::int64_t> pentagonalResolutionParameterM{"pentagonalResolutionParameterM"};
    const KV<std::int64_t> j{"J"};
    const KV<std::int64_t> k{"K"};
    const KV<std::int64_t> m{"M"};
    const KV<std::int64_t> subSetJ{"subSetJ"};
    const KV<std::int64_t> subSetK{"subSetK"};
    const KV<std::int64_t> subSetM{"subSetM"};
    const KV<std::int64_t> js{"JS"};
    const KV<std::int64_t> ks{"KS"};
    const KV<std::int64_t> ms{"MS"};


    // Regular ll
    const KV<std::int64_t> ni{"Ni"};
    const KV<std::int64_t> nj{"Nj"};

    // Regular ll - mapped
    const KV<double> north{"north"};
    const KV<double> west{"west"};
    const KV<double> south{"south"};
    const KV<double> east{"east"};
    const KV<double> westEastIncrement{"west_east_increment"};
    const KV<double> southNorthIncrement{"south_north_increment"};

    // Regular ll - direct
    const KV<double> latitudeOfFirstGridPointInDegrees{"latitudeOfFirstGridPointInDegrees"};
    const KV<double> latitudeOfLastGridPointInDegrees{"latitudeOfLastGridPointInDegrees"};
    const KV<double> longitudeOfFirstGridPointInDegrees{"longitudeOfFirstGridPointInDegrees"};
    const KV<double> longitudeOfLastGridPointInDegrees{"longitudeOfLastGridPointInDegrees"};
    const KV<double> jDirectionIncrementInDegrees{"jDirectionIncrementInDegrees"};
    const KV<double> iDirectionIncrementInDegrees{"iDirectionIncrementInDegrees"};


    static const Glossary& instance() {
        static Glossary glossary;
        return glossary;
    }
};

const Glossary& glossary();


//-----------------------------------------------------------------------------

}  // namespace multio::message
