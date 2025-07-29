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

#include "multio/datamod/DataModelling.h"
#include "multio/mars2grib/EncoderConf.h"

#include <iostream>
#include <memory>

// TODO: This is using the datamod enum key mechanism to set things.
//       The overall number of keys is not to much. After C++ migration (when we do not need to write to a local
//       configuration), The setters will set keys of a struct

namespace multio::mars2grib::rules {

struct DynSetter {
    // Combines matching and setting. If matched on `keys`, the setter is applied and true is returned.
    // If nothing matches only false is returned
    virtual void set(EncoderSections&) const = 0;
    virtual void print(std::ostream&) const = 0;

    virtual ~DynSetter() = default;
};


// First id_ is the key to be set. idx is the path to it
struct NoOp : DynSetter {
    void set(EncoderSections& conf) const override {}
    void print(std::ostream& os) const override { os << "NoOp"; };
};


// First id_ is the key to be set. idx is the path to it
template <auto id_, auto... idx>
struct SetKey : DynSetter {
    datamod::KeyValue<id_> value;

    void set(EncoderSections& conf) const override { datamod::alteredKeyPath<idx..., id_>(conf) = value; }
    void print(std::ostream& os) const override {
        os << "SetKey(";
        util::print(os, value);
        os << ")";
    }
};

// Constructor for SetKey - if no argument is passed it means that we set the value (even when it is marked as optional)
// and alter
template <auto id_, auto... idx>
SetKey<id_, idx...> setKey(datamod::KeyValue<id_> val = datamod::KeyValue<id_>{datamod::KeyDefValueType_t<id_>{}}) {
    alter(val);
    SetKey<id_, idx...> ret;
    ret.value = std::move(val);
    return ret;
}


struct SetAll : DynSetter {
    std::vector<std::unique_ptr<DynSetter>> setters;

    void set(EncoderSections& conf) const override {
        for (const auto& dynSetter : setters) {
            dynSetter.get()->set(conf);
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

}  // namespace multio::mars2grib::rules


