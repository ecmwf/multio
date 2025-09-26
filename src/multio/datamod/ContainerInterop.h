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
#include "eckit/config/LocalConfiguration.h"
#include "metkit/codes/CodesDecoder.h"

#include "multio/datamod/core/DataModellingException.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"

#include "multio/datamod/core/Record.h"
#include "multio/message/BaseMetadata.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"

#include "multio/util/MioGribHandle.h"
#include "multio/util/TypeToString.h"
#include "multio/util/TypeTraits.h"

namespace multio::datamod {

//-----------------------------------------------------------------------------
// Reading from metadata
//-----------------------------------------------------------------------------

template <>
struct EntryParser<message::BaseMetadata> {
    template <typename Rec>
    static void checkForNoAdditionalKeys(const message::BaseMetadata& md, const Rec& rec) {
        for (const auto& p : md) {
            if (!containsKey(p.first.value(), rec)) {
                std::ostringstream oss;
                oss << "The passed metadata contains an unexpected key " << p.first.value() << " in metadata " << md;
                throw DataModellingException(oss.str(), Here());
            }
        }
    }

    static void throwRequiredKeyIsNull(const std::string& keyInfo, const message::BaseMetadata& md) {
        std::ostringstream oss;
        oss << "Required key " << keyInfo << " is null in metadata " << md;
        throw DataModellingException(oss.str(), Here());
    }
    static void throwMissingRequiredKey(const std::string& keyInfo, const message::BaseMetadata& md) {
        std::ostringstream oss;
        oss << "Missing required key " << keyInfo << " in metadata " << md;
        throw DataModellingException(oss.str(), Here());
    }
    static void throwUnsupportedType(const std::string& typeStr, const std::string& keyInfo,
                                     const message::BaseMetadata& md) {
        std::ostringstream oss;
        oss << "Unsupported type " << typeStr << " for key " << keyInfo << " in metadata: " << md;
        throw DataModellingException(oss.str(), Here());
    }


    template <bool byRef = true, typename EntryDef_,
              std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_>), bool> = true>
    static decltype(auto) makeVisitor(const EntryDef_& entryDef, const message::BaseMetadata& md) noexcept {
        using TP = typename EntryDef_::ParserDumper;
        using Ret = EntryType_t<EntryDef_>;

        return [&](auto&& v) -> Ret {
            if constexpr (std::is_same_v<message::Null, std::decay_t<decltype(v)>>) {
                if constexpr (EntryDef_::tag == EntryTag::Required) {
                    throwRequiredKeyIsNull(entryDef.keyInfo(), md);
                }
                return {};
            }
            else if constexpr (TP::template CanCreateFromValue_v<decltype(v)>) {
                if constexpr (byRef) {
                    return entryDef.makeEntryRef(std::forward<decltype(v)>(v));
                }
                else {
                    return entryDef.makeEntry(std::forward<decltype(v)>(v));
                }
            }
            else {
                throwUnsupportedType(util::typeToString<std::decay_t<decltype(v)>>(), entryDef.keyInfo(), md);
            }
            return {};  // unreachable
        };
    }


    template <typename EntryDef_, std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_>), bool> = true>
    static EntryType_t<EntryDef_> defaultOrThrow(const EntryDef_& entryDef, const message::BaseMetadata& md) {
        if constexpr (EntryDef_::tag == EntryTag::Required) {
            throwMissingRequiredKey(entryDef.keyInfo(), md);
        }
        return {};
    }


    template <
        typename EntryDef_, typename MD,
        std::enable_if_t<
            (IsBaseEntryDefinition_v<EntryDef_> && std::is_base_of_v<message::BaseMetadata, std::decay_t<MD>>), bool>
        = true>
    static EntryType_t<EntryDef_> getByRef(const EntryDef_& entryDef, MD&& md, const ParseOptions&) {
        if (auto search = std::forward<MD>(md).localFind(entryDef.key()); search != md.end()) {
            if constexpr (std::is_lvalue_reference_v<MD>) {
                return search->second.visit(makeVisitor(entryDef, md));
            }
            else {
                return std::move(search->second).visit(makeVisitor(entryDef, md));
            }
        }
        return defaultOrThrow(entryDef, md);
    }

    template <
        typename EntryDef_, typename MD,
        std::enable_if_t<
            (IsBaseEntryDefinition_v<EntryDef_> && std::is_base_of_v<message::BaseMetadata, std::decay_t<MD>>), bool>
        = true>
    static EntryType_t<EntryDef_> getByValue(const EntryDef_& entryDef, MD&& md, const ParseOptions& opts) {
        return getByValueThroughRef<message::BaseMetadata>(entryDef, std::forward<MD>(md), opts);
    }
};

// The implementation for Metadata explicitly allows reading from the
// global parametrization. Global values may be just referenced (i.e. arrays like PV array)
template <>
struct EntryParser<message::Metadata> {
    using BaseParser = EntryParser<message::BaseMetadata>;

    template <typename Rec>
    static void checkForNoAdditionalKeys(const message::Metadata& md, const Rec& rec) {
        BaseParser::checkForNoAdditionalKeys(md, rec);
    }

    template <typename EntryDef_, typename MD,
              std::enable_if_t<
                  (IsBaseEntryDefinition_v<EntryDef_> && std::is_base_of_v<message::Metadata, std::decay_t<MD>>), bool>
              = true>
    static EntryType_t<EntryDef_> getByRef(const EntryDef_& entryDef, MD&& md, const ParseOptions&) {
        if (auto search = std::forward<MD>(md).localFind(entryDef.key()); search != md.end()) {
            if constexpr (std::is_lvalue_reference_v<MD>) {
                return search->second.visit(BaseParser::makeVisitor(entryDef, md));
            }
            else {
                return std::move(search->second).visit(BaseParser::makeVisitor(entryDef, md));
            }
        }
        // Then do manual search on parametrization
        const auto& global = message::Parametrization::instance().get();
        if (auto search = global.localFind(entryDef.key()); search != global.end()) {
            return search->second.visit(BaseParser::makeVisitor(entryDef, md));
        }
        return BaseParser::defaultOrThrow(entryDef, md);
    }

    // The intention is to always store values from global parametrization as reference.
    // Values from metadata are copied while values from global metadata are referenced
    template <typename EntryDef_, typename MD,
              std::enable_if_t<
                  (IsBaseEntryDefinition_v<EntryDef_> && std::is_base_of_v<message::Metadata, std::decay_t<MD>>), bool>
              = true>
    static EntryType_t<EntryDef_> getByValue(const EntryDef_& entryDef, MD&& md, const ParseOptions&) {
        if (auto search = std::forward<MD>(md).localFind(entryDef.key()); search != md.end()) {
            if constexpr (std::is_lvalue_reference_v<MD>) {
                return search->second.visit(BaseParser::makeVisitor<false>(entryDef, md));
            }
            else {
                return std::move(search->second).visit(BaseParser::makeVisitor<false>(entryDef, md));
            }
        }

        // Then do manual search on parametrization
        const auto& global = message::Parametrization::instance().get();
        if (auto search = global.localFind(entryDef.key()); search != global.end()) {
            return search->second.visit(BaseParser::makeVisitor<true>(entryDef, md));
        }
        return BaseParser::defaultOrThrow(entryDef, md);
    }
};


//-----------------------------------------------------------------------------
// Writing to metadata
//-----------------------------------------------------------------------------

template <>
struct EntryDumper<message::BaseMetadata> {
    template <typename EntryDef_, typename Entry_,
              std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_> && IsEntry_v<std::decay_t<Entry_>>), bool> = true>
    static void set(const EntryDef_& entryDef, Entry_&& entry, message::BaseMetadata& md, const DumpOptions&) {
        using TP = typename EntryDef_::ParserDumper;
        // TODO pgeier think about handling missing value by setting Null ?
        std::forward<Entry_>(entry).visit(  //
            eckit::Overloaded{[&](UnsetType v) { md.erase(entryDef.key()); },
                              [&](auto&& v) {
                                  // The contained value might be or mapped to a variant, that's
                                  // why we visit
                                  TP::template dumpToAndVisit<message::BaseMetadata>(
                                      std::forward<decltype(v)>(v),
                                      [&](auto&& vi) { md.set(entryDef.key(), std::forward<decltype(vi)>(vi)); });
                              }});
    }
};

template <>
struct EntryDumper<message::Metadata> : EntryDumper<message::BaseMetadata> {
    using Base = EntryDumper<message::BaseMetadata>;
    using Base::set;
};


//-----------------------------------------------------------------------------
// Reading from LocalConfiguration
//-----------------------------------------------------------------------------

template <>
struct EntryParser<eckit::Configuration> {
    template <typename Rec>
    static void checkForNoAdditionalKeys(const eckit::Configuration& conf, const Rec& rec) {
        auto keys = conf.keys();

        // Remove all keys for which containsKey returns true
        keys.erase(std::remove_if(keys.begin(), keys.end(), [&](const auto& key) { return containsKey(key, rec); }),
                   keys.end());

        if (keys.size() > 0) {
            std::ostringstream oss;
            oss << "The passed configuration contains more keys than expected: " << keys << " in configuration "
                << conf;
            throw DataModellingException(oss.str(), Here());
        }
    }

    template <typename T>
    static T getValueByType(const eckit::Configuration& c, const std::string& key) {
        T val;
        c.get(key, val);
        return val;
    }


    template <typename Func>
    static decltype(auto) visitNonNullValue(const std::string& key, const eckit::Configuration& c, Func&& func) {
        // Ridiculous chain of "reflective" calls
        if (c.isBoolean(key)) {
            return std::forward<Func>(func)(util::TypeTag<bool>{});
        }
        if (c.isBooleanList(key)) {
            return std::forward<Func>(func)(util::TypeTag<std::vector<std::int64_t>>{});
        }
        if (c.isFloatingPoint(key)) {
            return std::forward<Func>(func)(util::TypeTag<double>{});
        }
        if (c.isFloatingPointList(key)) {
            return std::forward<Func>(func)(util::TypeTag<std::vector<double>>{});
        }
        if (c.isIntegral(key)) {
            return std::forward<Func>(func)(util::TypeTag<std::int64_t>{});
        }
        if (c.isIntegralList(key)) {
            return std::forward<Func>(func)(util::TypeTag<std::vector<double>>{});
        }
        // if (c.isList(key)) {
        //     // Not supported
        // }
        if (c.isString(key)) {
            return std::forward<Func>(func)(util::TypeTag<std::string>{});
        }
        if (c.isStringList(key)) {
            return std::forward<Func>(func)(util::TypeTag<std::vector<std::string>>{});
        }
        if (c.isSubConfiguration(key)) {
            return std::forward<Func>(func)(util::TypeTag<eckit::LocalConfiguration>{});
        }

        return std::forward<Func>(func)();
    }

    static void throwHasNoKey(const std::string& keyInfo, const eckit::Configuration& conf) {
        std::ostringstream oss;
        oss << "eckit::Configuration has no key " << keyInfo << ": " << conf << std::endl;
        throw DataModellingException(oss.str(), Here());
    }

    static void throwIsNull(const std::string& keyInfo, const eckit::Configuration& conf) {
        std::ostringstream oss;
        oss << "Key \"" << keyInfo << "\" in eckit::Configuration should have a non-null value: " << conf;
        throw DataModellingException(oss.str(), Here());
    }

    static void throwUnsupportedValue(const std::string& keyInfo, const eckit::Configuration& conf) {
        std::ostringstream oss;
        oss << "Unsupported value for key " << keyInfo << " in eckit::Configuration: " << conf;
        throw DataModellingException(oss.str(), Here());
    }

    static void throwUnsupportedType(const std::string& typeStr, const std::string& keyInfo,
                                     const eckit::Configuration& conf) {
        std::ostringstream oss;
        oss << "Unsupported type " << typeStr << " for key " << keyInfo << " in eckit::Configuration: " << conf;
        throw DataModellingException(oss.str(), Here());
    }


    template <typename EntryDef_, std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_>), bool> = true>
    static EntryType_t<EntryDef_> getByRef(const EntryDef_& entryDef, const eckit::Configuration& conf,
                                           const ParseOptions&) {
        using TP = typename EntryDef_::ParserDumper;
        using Ret = EntryType_t<EntryDef_>;
        std::string key{entryDef.key()};
        if (!conf.has(key)) {
            if constexpr (EntryDef_::tag == EntryTag::Required) {
                throwHasNoKey(entryDef.keyInfo(), conf);
            }
            return {};
        }

        if (conf.isNull(key)) {
            if constexpr (EntryDef_::tag == EntryTag::Required) {
                throwIsNull(entryDef.keyInfo(), conf);
            }
            return {};
        }


        return visitNonNullValue(key, conf,
                                 eckit::Overloaded{[&]() -> Ret {
                                                       throwUnsupportedValue(entryDef.keyInfo(), conf);
                                                       return {};  // unreachable
                                                   },
                                                   [&](auto tt) -> Ret {
                                                       using Type = typename std::decay_t<decltype(tt)>::type;
                                                       if constexpr (TP::template CanCreateFromValue_v<Type>) {
                                                           return entryDef.makeEntry(getValueByType<Type>(conf, key));
                                                       }
                                                       else {
                                                           throwUnsupportedType(util::typeToString<Type>(),
                                                                                entryDef.keyInfo(), conf);
                                                       }
                                                       return {};  // unreachable
                                                   }});
    }


    template <
        typename EntryDef_, typename Conf,
        std::enable_if_t<
            (IsBaseEntryDefinition_v<EntryDef_> && std::is_base_of_v<eckit::Configuration, std::decay_t<Conf>>), bool>
        = true>
    static EntryType_t<EntryDef_> getByValue(const EntryDef_& entryDef, Conf&& conf, const ParseOptions& opts) {
        return getByValueThroughRef<eckit::Configuration>(entryDef, std::forward<Conf>(conf), opts);
    }
};


template <>
struct EntryParser<eckit::LocalConfiguration> : EntryParser<eckit::Configuration> {
    using Base = EntryParser<eckit::Configuration>;
    using Base::checkForNoAdditionalKeys;
    using Base::getByRef;
    using Base::getByValue;
};


//-----------------------------------------------------------------------------
// Writing to LocalConfiguration
//-----------------------------------------------------------------------------

template <>
struct EntryDumper<eckit::LocalConfiguration> {
    template <
        typename EntryDef_, typename Entry_,
        std::enable_if_t<(IsBaseEntryDefinition_v<std::decay_t<EntryDef_>> && IsEntry_v<std::decay_t<Entry_>>), bool>
        = true>
    static void set(const EntryDef_& entryDef, Entry_&& entry, eckit::LocalConfiguration& conf,
                    const DumpOptions& dump) {
        std::forward<Entry_>(entry).visit(eckit::Overloaded{
            [&](UnsetType v) {
                std::string key{entryDef.key()};
                if (conf.has(key)) {
                    conf.remove(key);
                }
            },
            [&](auto&& v) {
                // The contained value might be or mapped to a variant, that's why we visit
                EntryDef_::ParserDumper::template dumpToAndVisit<eckit::LocalConfiguration>(
                    std::forward<decltype(v)>(v),
                    [&](auto&& vi) { conf.set(std::string(entryDef.key()), std::forward<decltype(vi)>(vi)); });
            }});
    }
};


//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Reading from MioGribHandle
//-----------------------------------------------------------------------------

template <>
struct EntryParser<util::MioGribHandle> {

    static void throwRequiredKeyDefinedButMissing(const std::string& keyInfo) {
        std::ostringstream oss;
        oss << "Required key " << keyInfo << " is defined but missing on eccodes handle.";
        throw DataModellingException(oss.str(), Here());
    }

    static void throwRequiredKeyNotDefined(const std::string& keyInfo) {
        std::ostringstream oss;
        oss << "Required key " << keyInfo << " is not defined on eccodes handle.";
        throw DataModellingException(oss.str(), Here());
    }

    static void throwWrongType(const std::string& keyInfo, const std::string& typeStr) {
        std::ostringstream oss;
        oss << "Can not create key " << keyInfo << " from " << typeStr;
        throw DataModellingException(oss.str(), Here());
    }


    template <typename EntryDef_, std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_>), bool> = true>
    static EntryType_t<EntryDef_> getByRef(const EntryDef_& entryDef, const util::MioGribHandle& handle,
                                           const ParseOptions&) {
        using TP = typename EntryDef_::ParserDumper;
        using ValueType = typename EntryDef_::ValueType;

        // TODO pgeier use const char* in EntryDef for better IO performance?
        std::string key{entryDef.key()};

        // For codes we always copy - no value by ref
        if (!handle.isDefined(key)) {
            if constexpr (EntryDef_::tag == EntryTag::Required) {
                throwRequiredKeyNotDefined(entryDef.keyInfo());
            }
            return {};
        }
        if (handle.isMissing(key)) {
            if constexpr (EntryDef_::tag == EntryTag::Required) {
                throwRequiredKeyDefinedButMissing(entryDef.keyInfo());
            }
            return {};
        }

        // Eccodes types interface is - basic types are string, double, long and bytes. Bytes can be weird to
        // handle, for now we just decode as string. To check if an array is contained, we ideally need to check the
        // size...
        int keyType = 0;
        ASSERT(codes_get_native_type(handle.raw(), key.c_str(), &keyType) == 0);
        switch (keyType) {
            case GRIB_TYPE_LONG: {
                if constexpr (util::IsVector_v<ValueType>) {
                    if constexpr (TP::template CanCreateFromValue_v<std::vector<long>>) {
                        return entryDef.makeEntry(handle.getLongArray(key));
                    }
                    else {
                        throwWrongType(entryDef.keyInfo(), util::typeToString<std::vector<long>>());
                    }
                }
                else {
                    if constexpr (TP::template CanCreateFromValue_v<long>) {
                        return entryDef.makeEntry(handle.getLong(key));
                    }
                    else {
                        throwWrongType(entryDef.keyInfo(), util::typeToString<long>());
                    }
                }
            }
            case GRIB_TYPE_DOUBLE: {
                if constexpr (util::IsVector_v<ValueType>) {
                    if constexpr (TP::template CanCreateFromValue_v<std::vector<double>>) {
                        return entryDef.makeEntry(handle.getDoubleArray(key));
                    }
                    else {
                        throwWrongType(entryDef.keyInfo(), util::typeToString<std::vector<double>>());
                    }
                }
                else {
                    if constexpr (TP::template CanCreateFromValue_v<double>) {
                        return entryDef.makeEntry(handle.getDouble(key));
                    }
                    else {
                        throwWrongType(entryDef.keyInfo(), util::typeToString<double>());
                    }
                }
            }
            case GRIB_TYPE_BYTES:
            case GRIB_TYPE_STRING: {
                // TODO pgeier add support for string vectors?
                if constexpr (TP::template CanCreateFromValue_v<std::string>) {
                    return entryDef.makeEntry(handle.getString(key));
                }
                else if constexpr (std::is_integral_v<ValueType> && TP::template CanCreateFromValue_v<long>) {
                    return entryDef.makeEntry(handle.getLong(key));
                }
                else if constexpr (std::is_floating_point_v<ValueType> && TP::template CanCreateFromValue_v<double>) {
                    return entryDef.makeEntry(handle.getDouble(key));
                }
                else {
                    throwWrongType(entryDef.keyInfo(), util::typeToString<std::string>());
                }
            }
            default: {
                throwWrongType(entryDef.keyInfo(), std::to_string(keyType));
            }
        }
        return {};
    }

    template <typename EntryDef_, typename Conf, std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_>), bool> = true>
    static EntryType_t<EntryDef_> getByValue(const EntryDef_& entryDef, const util::MioGribHandle& gh,
                                             const ParseOptions& opts) {
        return getByValueThroughRef<util::MioGribHandle>(entryDef, gh, opts);
    }
};


//-----------------------------------------------------------------------------
// Writing to MioGribHandle
//-----------------------------------------------------------------------------

template <>
struct EntryDumper<util::MioGribHandle> {
    template <typename EntryDef_, typename Entry_, typename GH,
              std::enable_if_t<(IsBaseEntryDefinition_v<EntryDef_> && IsEntry_v<std::decay_t<Entry_>>
                                && std::is_base_of_v<util::MioGribHandle, std::decay_t<GH>>),
                               bool>
              = true>
    static void set(const EntryDef_& entryDef, Entry_&& entry, GH& handle, const DumpOptions& opts) {
        using TP = typename EntryDef_::ParserDumper;
        std::string key{entryDef.key()};
        std::forward<Entry_>(entry).visit(eckit::Overloaded{
            [&](UnsetType v) {
                if (opts.removeMissingKeys) {
                    if (handle.isDefined(key)) {
                        handle.setMissing(key);
                    }
                }
            },
            [&](auto&& v) {
                if (!handle.isDefined(key)) {
                    std::ostringstream oss;
                    oss << "Key " << entryDef.keyInfo() << " should be written but is not defined on  eccodes handle.";
                    throw DataModellingException(oss.str(), Here());
                }
                // The contained value might be or mapped to a variant, that's why we visit
                TP::template dumpToAndVisit<util::MioGribHandle>(std::forward<decltype(v)>(v), [&](auto&& vi) {
                    handle.setValue(key, std::forward<decltype(vi)>(vi));
                });
            }});
    }
};

}  // namespace multio::datamod
