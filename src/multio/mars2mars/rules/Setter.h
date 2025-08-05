/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */


#pragma once

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/KeySetDef.h"

#include "multio/mars2mars/MappingResult.h"
#include "multio/mars2mars/Mars2MarsException.h"

#include <iostream>
#include <memory>


namespace multio::mars2mars::rules {

//-----------------------------------------------------------------------------

struct DynSetter {
    // Combines matching and setting. If matched on `keys`, the setter is applied and true is returned.
    // If nothing matches only false is returned
    virtual void set(datamod::MarsKeyValueSet&, datamod::MiscKeyValueSet&, MappingResult&) const = 0;
    virtual void print(std::ostream&) const = 0;

    virtual ~DynSetter() = default;
};

//-----------------------------------------------------------------------------

struct SetScaleFactor : DynSetter {
    double value;
    SetScaleFactor(double val): value(val) {}

    void set(datamod::MarsKeyValueSet&, datamod::MiscKeyValueSet&, MappingResult& mapRes) const override {
        if (mapRes.valuesScaleFactor) {
            std::ostringstream oss;
            print(oss);
            oss << ": valuesScaleFactor has already been set to: " << value;
            throw Mars2MarsException(oss.str(), Here());
        };
        mapRes.valuesScaleFactor = value;
    }
    void print(std::ostream& os) const override {
        os << "SetScaleFactor(";
        util::print(os, value);
        os << ")";
    }
};


//-----------------------------------------------------------------------------

// First id_ is the key to be set. idx is the path to it
template <auto id_>
struct SetKey : DynSetter {
    using KeySet = datamod::KeySet<decltype(id_)>;

    datamod::KeyValue<id_> value;

    void set(datamod::MarsKeyValueSet& marsVals, datamod::MiscKeyValueSet& miscVals, MappingResult&) const override {
        if constexpr (std::is_same_v<datamod::MarsKeyValueSet, datamod::KeyValueSet<KeySet>>) {
            datamod::key<id_>(marsVals) = value;
        }
        if constexpr (std::is_same_v<datamod::MiscKeyValueSet, datamod::KeyValueSet<KeySet>>) {
            datamod::key<id_>(miscVals) = value;
        }
    }
    void print(std::ostream& os) const override {
        os << "SetKey(";
        util::print(os, value);
        os << ")";
    }
};

template <auto id_>
SetKey<id_> setKey(datamod::KeyValue<id_> val = datamod::KeyValue<id_>{}) {
    alter(val);
    SetKey<id_> ret;
    ret.value = std::move(val);
    return ret;
}
template <auto id_>
SetKey<id_> setKey(datamod::KeyDefValueType_t<id_> val) {
    return setKey(datamod::KeyValue<id_>{val});
}

//-----------------------------------------------------------------------------

struct SetAll : DynSetter {
    std::vector<std::unique_ptr<DynSetter>> setters;

    void set(datamod::MarsKeyValueSet& marsVals, datamod::MiscKeyValueSet& miscVals,
             MappingResult& mapRes) const override {
        for (const auto& dynSetter : setters) {
            dynSetter.get()->set(marsVals, miscVals, mapRes);
        }
    }
    void print(std::ostream& os) const override {
        os << "setAll(";
        bool first = true;
        for (const auto& dynSetter : setters) {
            if (first) {
                first = false;
            }
            else {
                os << ", ";
            }
            dynSetter.get()->print(os);
        }
        os << ")";
    }
};

template <typename... Setters>
auto setAll(Setters&&... setters) {
    SetAll res;
    (res.setters.emplace_back(std::make_unique<std::decay_t<Setters>>(std::forward<Setters>(setters))), ...);
    return res;
}

}  // namespace multio::mars2mars::rules

