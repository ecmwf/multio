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

#include "multio/message/Metadata.h"
#include "multio/message/MetadataTypes.h"
#include "multio/message/MetadataValue.h"

#include <string>

namespace multio::message {

template <typename Description, typename ContainerRef_, bool isOptional_>
struct KeyValueAccess;


/**
 * TBD Create a typed decsription of metadata entries through which access on metadata is performed.
 */
template <typename ValidTypes_>
// template <typename Traits_, typename ValidTypes_, typename ParsedType,
//           typename ReadFunc = util::TranslateTo<ParsedType>, typename WriteFunc = util::Identity>
struct KeyValueDescription {
    using KeyType = typename MetadataTypes::KeyType;
    using ValidTypes = ValidTypes_;
    // using This = KeyValueDescription<Traits_, ValidTypes_, ParsedType, ReadFunc, WriteFunc>;
    using This = KeyValueDescription<ValidTypes_>;

    KeyType key;

    // ReadFunc read;
    // WriteFunc write;

    operator KeyType&() { return key; }
    operator const KeyType&() const { return key; }


    template <typename MD>
    decltype(auto) requiredOn(MD&& md) {
        return KeyValueAccess<This, MD, false>{*this, std::forward<MD>(md)};
    }

    template <typename MD>
    decltype(auto) optionalOn(MD&& md) {
        return KeyValueAccess<This, MD, true>{*this, std::forward<MD>(md)};
    }
};

// template <typename Traits, typename ValidTypes, typename ParsedType, typename Key>
// decltype(auto) keyDescription(Key&& key) noexcept {
//     return KeyValueDescription<Traits, ValidTypes, ParsedType>{std::forward<Key>(key)};
// }

// template <typename Traits, typename ValidTypes, typename ParsedType, typename Key, typename ReadFunc>
// decltype(auto) keyDescription(Key&& key, ReadFunc&& read) noexcept {
//     return KeyValueDescription<Traits, ValidTypes, ParsedType, ReadFunc>{std::forward<Key>(key),
//                                                                          std::forward<ReadFunc>(read)};
// }

// template <typename Traits, typename ValidTypes, typename ParsedType, typename Key, typename ReadFunc,
//           typename WriteFunc>
// decltype(auto) keyDescription(Key&& key, ReadFunc&& read, WriteFunc&& write) noexcept {
//     return KeyValueDescription<Traits, ValidTypes, ParsedType, ReadFunc, WriteFunc>{
//         std::forward<Key>(key), std::forward<ReadFunc>(read), std::forward<WriteFunc>(write)};
// }
// template <typename Traits, typename ValidTypes, typename ParsedType, typename Key>
// decltype(auto) keyDescription(Key&& key) noexcept {
//     return KeyValueDescription<Traits, ValidTypes, ParsedType>{std::forward<Key>(key)};
// }

// template <typename Traits, typename ValidTypes, typename ParsedType, typename Key, typename ReadFunc>
// decltype(auto) keyDescription(Key&& key, ReadFunc&& read) noexcept {
//     return KeyValueDescription<Traits, ValidTypes, ParsedType, ReadFunc>{std::forward<Key>(key),
//                                                                          std::forward<ReadFunc>(read)};
// }

// template <typename Traits, typename ValidTypes, typename ParsedType, typename Key, typename ReadFunc,
//           typename WriteFunc>
// decltype(auto) keyDescription(Key&& key, ReadFunc&& read, WriteFunc&& write) noexcept {
//     return KeyValueDescription<Traits, ValidTypes, ParsedType, ReadFunc, WriteFunc>{
//         std::forward<Key>(key), std::forward<ReadFunc>(read), std::forward<WriteFunc>(write)};
// }


template <typename Description, typename ContainerRef_, bool isOptional_>
struct KeyValueAccess {
    using Types = MetadataTypes;
    using ValidTypes
        = std::conditional_t<isOptional_,
                             util::MergeTypeList_t<typename Types::Nulls, typename Description::ValidTypes>,
                             typename Description::ValidTypes>;
    using ContainerRef = ContainerRef_;

    const Description& description;
    ContainerRef containerRef;  // Can be an temporary ref

    static const bool isOptional = isOptional_;


    // Internals -- Null ref to allow optionally passing null instead of a value.
    // The user will never see the MetadataValue itself but just the specific dispatched null type.
    // Hence
    MetadataValue nullRef{};

    using LookUpRef = util::InheritConstRef_t<ContainerRef, MetadataValue>;
    LookUpRef lookUp() && {
        if (auto searchKey = containerRef.find(description.key); searchKey != containerRef.end()) {
            return static_cast<LookUpRef>(searchKey->second);
        }
        if (isOptional) {
            // Return nullRef - static cast assures returning rvalue if expected
            return static_cast<LookUpRef>(nullRef);
        }
        else {
            throw MetadataMissingKeyException(description.key, Here());
        }
    }
};


// template <typename RetType, typename KeyType, typename IndexSeq, typename ValidTypesListOfList, typename Func>
// struct VariadicKeyValidator;

// template <typename RetType, typename KeyType, std::size_t... I, typename ListOfList, typename Func>
// struct VariadicKeyValidator<RetType, KeyType, std::index_sequence<I...>, ListOfList, Func> {
//     std::array<std::reference_wrapper<const KeyType>, sizeof...(I)> keys;
//     Func func;


//     template <typename... Args>
//     RetType operator()(Args&&... args) && {
//         if constexpr ((true && ...
//                        && util::TypeListContains<util::GetIthType_t<I, util::TypeList<std::decay_t<Args>...>>,
//                                                  util::GetIthType_t<I, ListOfList>>::value)) {
//             return std::move(func)(std::forward<Args>(args)...);
//         }
//         else {
//             std::ostringstream oss;
//             oss << "Type validation failed for keys : ";
//             (oss << ...
//                  << (util::TypeListContains<util::GetIthType_t<I, util::TypeList<std::decay_t<Args>...>>,
//                                             util::GetIthType_t<I, ListOfList>>::value
//                          ? std::string("")
//                          : (std::string(std::get<I>(keys).get()) + std::string(", "))));

//             throw MetadataException(oss.str(), Here());
//         };
//     }
// };

template <typename RetType, typename Func>
struct VariadicKeyValidator {
    Func func;


    template <typename Tup2>
    static decltype(auto) concatTup(Null, Tup2&& tup2) {
        return std::forward<Tup2>(tup2);
    }

    template <typename Tup1, typename Tup2>
    static decltype(auto) concatTup(Tup1&& tup1, Tup2&& tup2) {
        return std::tuple_cat(std::forward<Tup1>(tup1), std::forward<Tup2>(tup2));
    }


    template <typename Tupl>
    RetType handleArgByArg(Tupl&& tupl) {
        return std::apply(std::move(func), std::forward<Tupl>(tupl));
    }

    template <typename TuplOrNull, typename Arg, typename... Args>
    RetType handleArgByArg(TuplOrNull&& tupl, Arg&& arg, Args&&... args) {
        return util::visitUnwrapUniquePtr(
            [&](auto&& varg) -> RetType {
                using T = decltype(varg);
                if constexpr (util::TypeListContains<std::decay_t<T>, typename std::decay_t<Arg>::ValidTypes>::value) {
                    return handleArgByArg(concatTup(std::forward<TuplOrNull>(tupl),
                                                    std::tuple<decltype(varg)>{std::forward<decltype(varg)>(varg)}),
                                          std::forward<Args>(args)...);
                }
                else {
                    std::ostringstream oss;
                    oss << "Type validation failed for key: " << arg.description.key;
                    throw MetadataException(oss.str(), Here());
                }
            },
            std::forward<Arg>(arg).lookUp());
    }

    template <typename... Args>
    RetType operator()(Args&&... args) && {
        return handleArgByArg(Null{}, std::forward<Args>(args)...);
    }
};


template <typename RetType, typename Func, typename... KVAccesses>
RetType validateAll(Func&& func, KVAccesses&&... vals) {
    // using Traits = util::TypeListHead_t<util::TypeList<typename std::decay_t<KVAccesses>::Traits...>>;
    // using KeyType = typename Traits::KeyType;

    // return util::visitUnwrapUniquePtr(
    //     VariadicKeyValidator<RetType, KeyType, std::make_index_sequence<sizeof...(KVAccesses)>,
    //                          util::TypeList<typename std::decay_t<KVAccesses>::ValidTypes...>, Func>{
    //         {std::cref(vals.description.key)...}, std::forward<Func>(func)},
    //     std::forward<KVAccesses>(vals).lookUp()...);

    return VariadicKeyValidator<RetType, Func>{std::forward<Func>(func)}(std::forward<KVAccesses>(vals)...);
}

template <typename Func, typename... KVAccesses>
decltype(auto) validateAll(Func&& func, KVAccesses&&... vals) {
    using Ret = decltype(std::forward<Func>(func)(
        std::declval<util::InheritConstRef_t<KVAccesses, util::TypeListHead_t<typename KVAccesses::ValidTypes>>>()...));
    return validateAll<Ret>(std::forward<Func>(func), std::forward<KVAccesses>(vals)...);
}


/**
 * TBD Create a typed decsription of metadata entries through which access on metadata is performed.
 */
// template <typename KeyType, typename ValidTypes>
// struct KeyValueDescription {
//     KeyType key;

//     operator KeyType&() { return key; }
//     operator const KeyType&() const { return key; }
// };


//-----------------------------------------------------------------------------

/**
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

    // template <typename ValidTypes>
    // using Description = KeyValueDescription<KeyType, ValidTypes>;

    // General keys
    const KeyType name{"name"};
    const KeyType paramId{"paramId"};
    const KeyType param{"param"};
    const KeyType globalSize{"globalSize"};
    const KeyType domain{"domain"};
    const KeyType date{"date"};
    const KeyType time{"time"};
    const KeyType precision{"precision"};


    // Nemo
    const KeyType nemoParam{"nemoParam"};
    const KeyType category{"category"};

    // Mars keys
    const KeyType type{"type"};
    const KeyType marsType{"marsType"};
    const KeyType classKey{"class"};
    const KeyType marsClass{"marsClass"};
    const KeyType stream{"stream"};
    const KeyType marsStream{"marsStream"};
    const KeyType expver{"expver"};
    const KeyType experimentVersionNumber{"experimentVersionNumber"};
    const KeyType levelist{"levelist"};
    const KeyType levtype{"levtype"};
    const KeyType levtypeWam{"levtype_wam"};
    const KeyType dataset{"dataset"};
    const KeyType activity{"activity"};
    const KeyType experiment{"experiment"};
    const KeyType generation{"generation"};
    const KeyType model{"model"};
    const KeyType realization{"realization"};

    // Eccodes specific
    const KeyType gribEdition{"gribEdition"};
    const KeyType tablesVersion{"tablesVersion"};
    const KeyType setLocalDefinition{"setLocalDefinition"};
    const KeyType grib2LocalSectionNumber{"grib2LocalSectionNumber"};
    const KeyType productionStatusOfProcessedData{"productionStatusOfProcessedData"};

    // Eccodes concepts
    const KeyType gridType{"gridType"};
    const KeyType typeOfLevel{"typeOfLevel"};
    const KeyType localDefinitionNumber{"localDefinitionNumber"};

    // Additional eccodes keys
    const KeyType setPackingType{"setPackingType"};
    const KeyType complexPacking{"complexPacking"};
    const KeyType missingValue{"missingValue"};
    const KeyType bitsPerValue{"bitsPerValue"};
    const KeyType bitmapPresent{"bitmapPresent"};

    // Grib general
    const KeyType typeOfGeneratingProcess{"typeOfGeneratingProcess"};  // Analog to mars type
    const KeyType generatingProcessIdentifier{"generatingProcessIdentifier"};
    const KeyType subCentre{"subCentre"};

    const KeyType ensembleMember{"ensemble-member"};
    const KeyType ensembleSize{"ensemble-size"};

    // Eccodes grib reference date/time - direct setting (alternative to date & time)
    const KeyType year{"year"};
    const KeyType month{"month"};
    const KeyType day{"day"};
    const KeyType hour{"hour"};
    const KeyType minute{"minute"};
    const KeyType second{"second"};

    // Eccodes analysis date/time - direct setting (alternative to dateOfAnalysis & timeOfAnalysis) -- ONLY VALID FOR A
    // SPECIFIC localDefinitionNumber
    const KeyType yearOfAnalysis{"yearOfAnalysis"};
    const KeyType monthOfAnalysis{"monthOfAnalysis"};
    const KeyType dayOfAnalysis{"dayOfAnalysis"};
    const KeyType hourOfAnalysis{"hourOfAnalysis"};
    const KeyType minuteOfAnalysis{"minuteOfAnalysis"};
    const KeyType secondOfAnalysis{"secondOfAnalysis"};

    // Eccodes grib2 stat
    const KeyType yearOfEndOfOverallTimeInterval{"yearOfEndOfOverallTimeInterval"};
    const KeyType monthOfEndOfOverallTimeInterval{"monthOfEndOfOverallTimeInterval"};
    const KeyType dayOfEndOfOverallTimeInterval{"dayOfEndOfOverallTimeInterval"};
    const KeyType hourOfEndOfOverallTimeInterval{"hourOfEndOfOverallTimeInterval"};
    const KeyType minuteOfEndOfOverallTimeInterval{"minuteOfEndOfOverallTimeInterval"};
    const KeyType secondOfEndOfOverallTimeInterval{"secondOfEndOfOverallTimeInterval"};
    const KeyType typeOfStatisticalProcessing{"typeOfStatisticalProcessing"};
    const KeyType lengthOfTimeRange{"lengthOfTimeRange"};
    const KeyType indicatorOfUnitForTimeIncrement{"indicatorOfUnitForTimeIncrement"};
    const KeyType timeIncrement{"timeIncrement"};

    // Eccodes grib2 grid
    const KeyType unstructuredGridType{"unstructuredGridType"};
    const KeyType unstructuredGridSubtype{"unstructuredGridSubtype"};
    const KeyType uuidOfHGrid{"uuidOfHGrid"};

    // Eccodes grib horizontal + vertial
    const KeyType level{"level"};
    const KeyType scaledValueOfFirstFixedSurface{"scaledValueOfFirstFixedSurface"};
    const KeyType scaledValueOfSecondFixedSurface{"scaledValueOfSecondFixedSurface"};

    // Time model
    const KeyType startTime{"startTime"};
    const KeyType startDate{"startDate"};
    const KeyType previousTime{"previousTime"};
    const KeyType previousDate{"previousDate"};
    const KeyType currentTime{"currentTime"};
    const KeyType currentDate{"currentDate"};

    const KeyType sampleInterval{"sampleInterval"};
    const KeyType sampleIntervalInSeconds{"sampleIntervalInSeconds"};

    // legacy & conversion
    const KeyType timeStep{"timeStep"};
    const KeyType step{"step"};
    const KeyType stepRange{"stepRange"};
    const KeyType startStep{"startStep"};
    const KeyType endStep{"endStep"};
    const KeyType dataTime{"dataTime"};
    const KeyType dataDate{"dataDate"};

    const KeyType dateOfAnalysis{"date-of-analysis"};
    const KeyType timeOfAnalysis{"time-of-analysis"};

    // Statistic
    const KeyType operation{"operation"};
    const KeyType restartStep{"restart-step"};
    const KeyType stepFrequency{"step-frequency"};

    // Healpix
    const KeyType nside{"Nside"};
    const KeyType orderingConvention{"orderingConvention"};

    // Spherical harmonics
    const KeyType sphericalHarmonics{"sphericalHarmonics"};
    const KeyType pentagonalResolutionParameterJ{"pentagonalResolutionParameterJ"};
    const KeyType pentagonalResolutionParameterK{"pentagonalResolutionParameterK"};
    const KeyType pentagonalResolutionParameterM{"pentagonalResolutionParameterM"};
    const KeyType j{"J"};
    const KeyType k{"K"};
    const KeyType m{"M"};
    const KeyType subSetJ{"subSetJ"};
    const KeyType subSetK{"subSetK"};
    const KeyType subSetM{"subSetM"};
    const KeyType js{"JS"};
    const KeyType ks{"KS"};
    const KeyType ms{"MS"};


    // Regular ll
    const KeyType ni{"Ni"};
    const KeyType nj{"Nj"};
    const KeyType north{"north"};
    const KeyType west{"west"};
    const KeyType south{"south"};
    const KeyType east{"east"};
    const KeyType westEastIncrement{"west_east_increment"};
    const KeyType southNorthIncrement{"south_north_increment"};


    static const Glossary& instance() {
        static Glossary glossary;
        return glossary;
    }
};

const Glossary& glossary();

//-----------------------------------------------------------------------------

}  // namespace multio::message
