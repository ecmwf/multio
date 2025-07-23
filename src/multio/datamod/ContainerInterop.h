/*
 * (C) Copyright 1996- ECMWF.
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
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/DataModellingException.h"
#include "multio/datamod/core/KeyValue.h"
#include "multio/message/BaseMetadata.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/TypeToString.h"
#include "multio/util/TypeTraits.h"
#include "multio/util/VariantHelpers.h"

namespace multio::datamod {

//-----------------------------------------------------------------------------
// Reading from metadata
//-----------------------------------------------------------------------------

template <>
struct KeyValueReader<message::BaseMetadata> : BaseKeyValueReader<message::BaseMetadata> {
    using Base = BaseKeyValueReader<message::BaseMetadata>;
    using Base::getByRef;
    using Base::getByValue;

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


    template <bool byRef = true, typename KVD, std::enable_if_t<(IsDynamicKey_v<KVD>), bool> = true>
    static decltype(auto) makeVisitor(const KVD& kvd, const message::BaseMetadata& md) noexcept {
        using RW = typename KVD::ReadWrite;
        using Ret = KeyValueFromKey_t<KVD>;
        return [&](auto&& v) -> Ret {
            if constexpr (std::is_same_v<message::Null, std::decay_t<decltype(v)>>) {
                if constexpr (KVD::tag == KVTag::Required) {
                    throwRequiredKeyIsNull(kvd.keyInfo(), md);
                }
                return {};
            }
            else if constexpr (RW::template CanCreateFromValue_v<decltype(v)>) {
                if constexpr (byRef) {
                    return toKeyValueRef(kvd, std::forward<decltype(v)>(v));
                }
                else {
                    return toKeyValue(kvd, std::forward<decltype(v)>(v));
                }
            }
            else {
                throwUnsupportedType(util::typeToString<std::decay_t<decltype(v)>>(), kvd.keyInfo(), md);
            }
            return {};  // unreachable
        };
    }


    template <typename KVD, std::enable_if_t<(IsDynamicKey_v<KVD>), bool> = true>
    static KeyValueFromKey_t<KVD> defaultOrThrow(const KVD& kvd, const message::BaseMetadata& md) {
        using Ret = KeyValueFromKey_t<KVD>;
        if constexpr (KVD::tag == KVTag::Required) {
            throwMissingRequiredKey(kvd.keyInfo(), md);
        }
        return {};
    }


    template <
        typename KVD, typename MD,
        std::enable_if_t<(IsDynamicKey_v<KVD> && std::is_base_of_v<message::BaseMetadata, std::decay_t<MD>>), bool>
        = true>
    static KeyValueFromKey_t<KVD> getByRef(const KVD& kvd, MD&& md) {
        if (auto search = std::forward<MD>(md).localFind(kvd.key()); search != md.end()) {
            if constexpr (std::is_lvalue_reference_v<MD>) {
                return search->second.visit(makeVisitor(kvd, md));
            }
            else {
                return std::move(search->second).visit(makeVisitor(kvd, md));
            }
        }
        return defaultOrThrow(kvd, md);
    }
};

// The implementation for Metadata explicitly allows reading from the
// global parametrization. Global values may be just referenced (i.e. arrays like PV array)
template <>
struct KeyValueReader<message::Metadata> : BaseKeyValueReader<message::Metadata> {
    using Base = BaseKeyValueReader<message::Metadata>;
    using Base::getByRef;
    using Base::getByValue;

    using BaseReader = KeyValueReader<message::BaseMetadata>;

    template <typename KVD, typename MD,
              std::enable_if_t<(IsDynamicKey_v<KVD> && std::is_base_of_v<message::Metadata, std::decay_t<MD>>), bool>
              = true>
    static KeyValueFromKey_t<KVD> getByRef(const KVD& kvd, MD&& md) {
        if (auto search = std::forward<MD>(md).localFind(kvd.key()); search != md.end()) {
            if constexpr (std::is_lvalue_reference_v<MD>) {
                return search->second.visit(BaseReader::makeVisitor(kvd, md));
            }
            else {
                return std::move(search->second).visit(BaseReader::makeVisitor(kvd, md));
            }
        }
        // Then do manual search on parametrization
        const auto& global = message::Parametrization::instance().get();
        if (auto search = global.localFind(kvd.key()); search != global.end()) {
            return search->second.visit(BaseReader::makeVisitor(kvd, md));
        }
        return BaseReader::defaultOrThrow(kvd, md);
    }

    // The intention is to always store values from global parametrization as reference.
    // Values from metadata are copied while values from global metadata are referenced
    template <typename KVD, typename MD,
              std::enable_if_t<(IsDynamicKey_v<KVD> && std::is_base_of_v<message::Metadata, std::decay_t<MD>>), bool>
              = true>
    static KeyValueFromKey_t<KVD> getByValue(const KVD& kvd, MD&& md) {
        if (auto search = std::forward<MD>(md).localFind(kvd.key()); search != md.end()) {
            if constexpr (std::is_lvalue_reference_v<MD>) {
                return search->second.visit(BaseReader::makeVisitor<false>(kvd, md));
            }
            else {
                return std::move(search->second).visit(BaseReader::makeVisitor<false>(kvd, md));
            }
        }
        // Then do manual search on parametrization
        const auto& global = message::Parametrization::instance().get();
        if (auto search = global.localFind(kvd.key()); search != global.end()) {
            return search->second.visit(BaseReader::makeVisitor<true>(kvd, md));
        }
        return BaseReader::defaultOrThrow(kvd, md);
    }
};


//-----------------------------------------------------------------------------
// Writing to metadata
//-----------------------------------------------------------------------------

template <>
struct KeyValueWriter<message::BaseMetadata> : BaseKeyValueWriter<message::BaseMetadata> {
    using Base = BaseKeyValueWriter<message::BaseMetadata>;
    using Base::set;

    template <typename KVD, typename KV_,
              std::enable_if_t<(IsDynamicKey_v<std::decay_t<KVD>> && IsBaseKeyValue_v<std::decay_t<KV_>>), bool> = true>
    static void set(const KVD& kvd, KV_&& kv, message::BaseMetadata& md) {
        using RW = typename KVD::ReadWrite;
        // TODO think about handling missing value by setting Null ?
        std::forward<KV_>(kv).visit(  //
            eckit::Overloaded{[&](MissingValue v) {},
                              [&](auto&& v) {
                                  // The contained value might be or mapped to a variant, that's
                                  // why we visit
                                  RW::template writeAndVisit<message::BaseMetadata>(
                                      std::forward<decltype(v)>(v),
                                      [&](auto&& vi) { md.set(kvd.key(), std::forward<decltype(vi)>(vi)); });
                              }});
    }
};

template <>
struct KeyValueWriter<message::Metadata> : KeyValueWriter<message::BaseMetadata> {
    using Base = KeyValueWriter<message::BaseMetadata>;
    using Base::set;
};


//-----------------------------------------------------------------------------
// Reading from LocalConfiguration
//-----------------------------------------------------------------------------

template <>
struct KeyValueReader<eckit::Configuration> : BaseKeyValueReader<eckit::Configuration> {
    using Base = BaseKeyValueReader<eckit::Configuration>;
    using Base::getByRef;
    using Base::getByValue;


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


    template <typename KVD, std::enable_if_t<(IsDynamicKey_v<KVD>), bool> = true>
    static KeyValueFromKey_t<KVD> getByRef(const KVD& kvd, const eckit::Configuration& conf) {
        using Ret = KeyValueFromKey_t<KVD>;
        using RW = typename KVD::ReadWrite;
        if (!conf.has(kvd.key())) {
            if constexpr (KVD::tag == KVTag::Required) {
                throwHasNoKey(kvd.keyInfo(), conf);
            }
            return {};
        }

        if (conf.isNull(kvd.key())) {
            if constexpr (KVD::tag == KVTag::Required) {
                throwIsNull(kvd.keyInfo(), conf);
            }
            return {};
        }


        return visitNonNullValue(
            kvd.key(), conf,
            eckit::Overloaded{[&]() -> KeyValueFromKey_t<KVD> {
                                  throwUnsupportedValue(kvd.keyInfo(), conf);
                                  return {};  // unreachable
                              },
                              [&](auto tt) -> KeyValueFromKey_t<KVD> {
                                  using Type = typename std::decay_t<decltype(tt)>::type;
                                  if constexpr (RW::template CanCreateFromValue_v<Type>) {
                                      return toKeyValue(kvd, getValueByType<Type>(conf, kvd.key()));
                                  }
                                  else {
                                      throwUnsupportedType(util::typeToString<Type>(), kvd.keyInfo(), conf);
                                  }
                                  return {};  // unreachable
                              }});
    }
};


template <>
struct KeyValueReader<eckit::LocalConfiguration> : KeyValueReader<eckit::Configuration> {
    using Base = KeyValueReader<eckit::Configuration>;
    using Base::getByRef;
    using Base::getByValue;
};


//-----------------------------------------------------------------------------
// Writing to LocalConfiguration
//-----------------------------------------------------------------------------

template <>
struct KeyValueWriter<eckit::LocalConfiguration> : BaseKeyValueWriter<eckit::LocalConfiguration> {
    using Base = BaseKeyValueWriter<eckit::LocalConfiguration>;
    using Base::set;

    template <typename KVD, typename KV_,
              std::enable_if_t<(IsDynamicKey_v<std::decay_t<KVD>> && IsBaseKeyValue_v<std::decay_t<KV_>>), bool> = true>
    static void set(const KVD& kvd, KV_&& kv, eckit::LocalConfiguration& conf) {
        std::forward<KV_>(kv).visit(eckit::Overloaded{
            [&](MissingValue v) {},
            [&](auto&& v) {
                // The contained value might be or mapped to a variant, that's why we visit
                KVD::ReadWrite::template writeAndVisit<eckit::LocalConfiguration>(
                    std::forward<decltype(v)>(v),
                    [&](auto&& vi) { conf.set(std::string(kvd.key()), std::forward<decltype(vi)>(vi)); });
            }});
    }
};


//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Reading from MioGribHandle
//-----------------------------------------------------------------------------

template <>
struct KeyValueReader<util::MioGribHandle> : BaseKeyValueReader<util::MioGribHandle> {
    using Base = BaseKeyValueReader<util::MioGribHandle>;
    using Base::getByRef;
    using Base::getByValue;

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

    template <typename KVD, std::enable_if_t<(IsDynamicKey_v<KVD>), bool> = true>
    static KeyValueFromKey_t<KVD> getByRef(const KVD& kvd, const util::MioGribHandle& handle) {
        using RW = typename KVD::ReadWrite;
        using ValueType = typename KeyValueFromKey_t<KVD>::ValueType;
        using Ret = KeyValueFromKey_t<KVD>;

        // For codes we always copy - no value by ref
        if (!handle.isDefined(kvd.key())) {
            if constexpr (KVD::tag == KVTag::Required) {
                throwRequiredKeyNotDefined(kvd.keyInfo());
            }
            return {};
        }
        if (handle.isMissing(kvd.key())) {
            if constexpr (KVD::tag == KVTag::Required) {
                throwRequiredKeyDefinedButMissing(kvd.keyInfo());
            }
            return {};
        }

        // Eccodes types interface is - basic types are string, double, long and bytes. Bytes can be weird to
        // handle, for now we just decode as string. To check if an array is contained, we ideally need to check the
        // size...
        int keyType = 0;
        ASSERT(codes_get_native_type(handle.raw(), kvd.key().value().c_str(), &keyType) == 0);
        switch (keyType) {
            case GRIB_TYPE_LONG: {
                if constexpr (util::IsVector_v<KeyValueFromKey_t<KVD>>) {
                    if constexpr (RW::template CanCreateFromValue_v<std::vector<long>>) {
                        return toKeyValue(kvd, handle.getLongArray(kvd.key()));
                    }
                    else {
                        throwWrongType(kvd.keyInfo(), util::typeToString<std::vector<long>>());
                    }
                }
                else {
                    if constexpr (RW::template CanCreateFromValue_v<long>) {
                        return toKeyValue(kvd, handle.getLong(kvd.key()));
                    }
                    else {
                        throwWrongType(kvd.keyInfo(), util::typeToString<long>());
                    }
                }
            }
            case GRIB_TYPE_DOUBLE: {
                if constexpr (util::IsVector_v<KeyValueFromKey_t<KVD>>) {
                    if constexpr (RW::template CanCreateFromValue_v<std::vector<double>>) {
                        return toKeyValue(kvd, handle.getDoubleArray(kvd.key()));
                    }
                    else {
                        throwWrongType(kvd.keyInfo(), util::typeToString<std::vector<double>>());
                    }
                }
                else {
                    if constexpr (RW::template CanCreateFromValue_v<double>) {
                        return toKeyValue(kvd, handle.getDouble(kvd.key()));
                    }
                    else {
                        throwWrongType(kvd.keyInfo(), util::typeToString<double>());
                    }
                }
            }
            case GRIB_TYPE_BYTES:
            case GRIB_TYPE_STRING: {
                // TODO add support for string vectors?
                if constexpr (RW::template CanCreateFromValue_v<std::string>) {
                    return toKeyValue(kvd, handle.getString(kvd.key()));
                }
                else if constexpr (std::is_integral_v<ValueType> && RW::template CanCreateFromValue_v<long>) {
                    return toKeyValue(kvd, handle.getLong(kvd.key()));
                }
                else if constexpr (std::is_floating_point_v<ValueType> && RW::template CanCreateFromValue_v<double>) {
                    return toKeyValue(kvd, handle.getDouble(kvd.key()));
                }
                else {
                    throwWrongType(kvd.keyInfo(), util::typeToString<std::string>());
                }
            }
            default: {
                throwWrongType(kvd.keyInfo(), std::to_string(keyType));
            }
        }
        return {};
    }
};


//-----------------------------------------------------------------------------
// Writing to MioGribHandle
//-----------------------------------------------------------------------------

template <>
struct KeyValueWriter<util::MioGribHandle> : BaseKeyValueWriter<util::MioGribHandle> {
    using Base = BaseKeyValueWriter<util::MioGribHandle>;
    using Base::set;

    template <typename KVD, typename KV_, typename GH,
              std::enable_if_t<(IsDynamicKey_v<std::decay_t<KVD>> && IsBaseKeyValue_v<std::decay_t<KV_>>
                                && std::is_base_of_v<util::MioGribHandle, std::decay_t<GH>>),
                               bool>
              = true>
    static void set(const KVD& kvd, KV_&& kv, GH& handle) {
        using RW = typename KVD::ReadWrite;
        std::forward<KV_>(kv).visit(eckit::Overloaded{
            [&](MissingValue v) {},
            [&](auto&& v) {
                if (!handle.isDefined(kvd.key())) {
                    std::ostringstream oss;
                    oss << "Key " << kvd.keyInfo() << " should be written but is not defined on  eccodes handle.";
                    throw DataModellingException(oss.str(), Here());
                }
                // The contained value might be or mapped to a variant, that's why we visit
                RW::template writeAndVisit<util::MioGribHandle>(std::forward<decltype(v)>(v), [&](auto&& vi) {
                    handle.setValue(kvd.key(), std::forward<decltype(vi)>(vi));
                });
            }});
    }
};

}  // namespace multio::datamod
