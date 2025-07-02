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

#include "multio/action/encode-mtg2/EncoderConf.h"
#include "multio/datamod/DataModelling.h"

#include <iostream>


namespace multio::action::rules {

// First id_ is the key to be set. idx is the path to it
struct NoOp {
    void operator()(EncoderSections& conf) const {}
};


template <typename NoOp_, std::enable_if_t<std::is_same_v<NoOp_, NoOp>, bool> = true>
std::ostream& operator<<(std::ostream& os, NoOp_) {
    os << "NoOp{}";
    return os;
}


// First id_ is the key to be set. idx is the path to it
template <auto id_, auto... idx>
struct SetKey {
    datamod::KeyValue<id_> value;

    void operator()(EncoderSections& conf) const { datamod::alteredKeyPath<idx..., id_>(conf) = value; }
};

// Constructor for SetKey - if no argument is passed it means that we set the value (even when it is marked as optional)
// and alter
template <auto id_, auto... idx>
SetKey<id_, idx...> setKey(datamod::KeyValue<id_> val = datamod::KeyValue<id_>{datamod::KeyDefValueType_t<id_>{}}) {
    alter(val);
    return SetKey<id_, idx...>{std::move(val)};
}

template <auto id_, auto... idx>
std::ostream& operator<<(std::ostream& os, const SetKey<id_, idx...>& sk) {
    os << "SetKey(" << sk.value << ")";
    return os;
}


template <typename... Setters>
struct SetAll {
    std::tuple<Setters...> setters;

    void operator()(EncoderSections& conf) const {
        std::apply([&](const auto&... setter) { (setter(conf), ...); }, setters);
    }
};

template <typename... Setters>
auto setAll(Setters&&... setters) {
    return SetAll<std::decay_t<Setters>...>{std::make_tuple(std::forward<Setters>(setters)...)};
}


template <typename... Setters>
std::ostream& operator<<(std::ostream& os, const SetAll<Setters...>& sk) {
    os << "setAll(";
    std::apply([&](const auto&... setters) { ((os << setters << ", "), ...); }, sk.setters);
    os << ")";
    return os;
}


}  // namespace multio::action::rules

