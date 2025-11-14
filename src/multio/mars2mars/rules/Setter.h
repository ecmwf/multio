/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */


#pragma once

#include "multio/datamod/MarsMiscGeo.h"

#include "multio/mars2mars/MappingResult.h"
#include "multio/mars2mars/Mars2MarsException.h"

#include <iostream>
#include <memory>


namespace multio::mars2mars::rules {

namespace dm = multio::datamod;

//-----------------------------------------------------------------------------

struct DynSetter {
    // Combines matching and setting. If matched on `keys`, the setter is applied and true is returned.
    // If nothing matches only false is returned
    virtual void set(dm::FullMarsRecord&, dm::MiscRecord&, MappingResult&) const = 0;
    virtual void print(util::PrintStream&) const = 0;

    virtual ~DynSetter() = default;
};

//-----------------------------------------------------------------------------

struct SetScaleFactor : DynSetter {
    double value;
    SetScaleFactor(double val) : value(val) {}

    void set(dm::FullMarsRecord&, dm::MiscRecord&, MappingResult& mapRes) const override {
        if (mapRes.valuesScaleFactor) {
            std::ostringstream oss;
            util::PrintStream ps(oss);
            print(ps);
            ps << ": valuesScaleFactor has already been set to: " << value;
            throw Mars2MarsException(oss.str(), Here());
        };
        mapRes.valuesScaleFactor = value;
    }
    void print(util::PrintStream& ps) const override { ps << "SetScaleFactor(" << value << ")"; }
};


//-----------------------------------------------------------------------------

template <typename RecordType, typename ValueType>
struct SetKey;

// First id_ is the key to be set. idx is the path to it
template <typename MarsRec, typename ValueType>
struct SetMarsKey : DynSetter {
    SetMarsKey(ValueType MarsRec::* member, ValueType v) : member_{member}, value{std::move(v)} {}
    ValueType MarsRec::* member_;
    ValueType value;

    void set(dm::FullMarsRecord& marsVals, dm::MiscRecord&, MappingResult&) const override {
        marsVals.*member_ = value;
    }
    void print(util::PrintStream& ps) const override { ps << "SetMarsKey(" << value << ")"; }
};

template <typename ValueType>
struct SetKey<dm::MarsFieldId, ValueType> : SetMarsKey<dm::MarsFieldId, ValueType> {
    using SetMarsKey<dm::MarsFieldId, ValueType>::SetMarsKey;
};
template <typename ValueType>
struct SetKey<dm::MarsFieldDetails, ValueType> : SetMarsKey<dm::MarsFieldDetails, ValueType> {
    using SetMarsKey<dm::MarsFieldDetails, ValueType>::SetMarsKey;
};
template <typename ValueType>
struct SetKey<dm::MarsId, ValueType> : SetMarsKey<dm::MarsId, ValueType> {
    using SetMarsKey<dm::MarsId, ValueType>::SetMarsKey;
};

template <typename ValueType>
struct SetKey<dm::MiscRecord, ValueType> : DynSetter {
    SetKey(ValueType dm::MiscRecord::* member, ValueType v) : member_{member}, value{std::move(v)} {}
    ValueType dm::MiscRecord::* member_;
    ValueType value;

    void set(dm::FullMarsRecord&, dm::MiscRecord& miscVals, MappingResult&) const override {
        miscVals.*member_ = value;
    }
    void print(util::PrintStream& ps) const override { ps << "SetMiscKey(" << value << ")"; }
};

template <typename RecordType, typename ValueType, typename Val>
SetKey<RecordType, ValueType> setKey(ValueType RecordType::* member, Val&& val) {
    ValueType RecordType::* member_ = member;
    return SetKey<RecordType, ValueType>(member_, ValueType{std::forward<Val>(val)});
}


//-----------------------------------------------------------------------------

struct SetAll : DynSetter {
    std::vector<std::unique_ptr<DynSetter>> setters;

    void set(dm::FullMarsRecord& marsVals, dm::MiscRecord& miscVals, MappingResult& mapRes) const override {
        for (const auto& dynSetter : setters) {
            dynSetter.get()->set(marsVals, miscVals, mapRes);
        }
    }
    void print(util::PrintStream& ps) const override {
        ps << "setAll(";
        bool first = true;
        for (const auto& dynSetter : setters) {
            if (first) {
                first = false;
                ps << "  ";
            }
            else {
                ps << ", ";
            }
            {
                util::IndentGuard g(ps);
                dynSetter.get()->print(ps);
            }
        }
        ps << ")";
    }
};

template <typename... Setters>
auto setAll(Setters&&... setters) {
    SetAll res;
    (res.setters.emplace_back(std::make_unique<std::decay_t<Setters>>(std::forward<Setters>(setters))), ...);
    return res;
}

}  // namespace multio::mars2mars::rules


namespace multio::util {

template <>
struct Print<mars2mars::rules::DynSetter> {
    static void print(PrintStream& ps, const mars2mars::rules::DynSetter& r) { r.print(ps); };
};

template <>
struct Print<mars2mars::rules::SetScaleFactor> {
    static void print(PrintStream& ps, const mars2mars::rules::SetScaleFactor& r) { r.print(ps); };
};


template <typename RecordType, typename ValueType>
struct Print<mars2mars::rules::SetKey<RecordType, ValueType>> {
    static void print(PrintStream& ps, const mars2mars::rules::SetKey<RecordType, ValueType>& r) { r.print(ps); };
};


template <>
struct Print<mars2mars::rules::SetAll> {
    static void print(PrintStream& ps, const mars2mars::rules::SetAll& r) { r.print(ps); };
};


}  // namespace multio::util

