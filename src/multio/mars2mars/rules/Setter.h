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

// First id_ is the key to be set. idx is the path to it
template <typename EntryDef_>
struct SetMarsKey : DynSetter {
    SetMarsKey(std::reference_wrapper<const EntryDef_> e, dm::EntryType_t<EntryDef_> v) :
        entryDef_{std::move(e)}, value{std::move(v)} {}
    std::reference_wrapper<const EntryDef_> entryDef_;
    dm::EntryType_t<EntryDef_> value;

    void set(dm::FullMarsRecord& marsVals, dm::MiscRecord& miscVals, MappingResult&) const override {
        entryDef_.get().get(marsVals) = value;
    }
    void print(util::PrintStream& ps) const override { ps << "SetMarsKey(" << value << ")"; }
};

template <typename EntryDef_>
SetMarsKey<EntryDef_> setMarsKey(const EntryDef_& entryDef,
                                 dm::EntryType_t<EntryDef_> val = dm::EntryType_t<EntryDef_>{}) {
    entryDef.applyDefaults(val);
    return SetMarsKey<EntryDef_>{std::cref(entryDef), std::move(val)};
}
template <typename EntryDef_>
SetMarsKey<EntryDef_> setMarsKey(const EntryDef_& entryDef, dm::EntryValueType_t<EntryDef_> val) {
    return setMarsKey(entryDef, dm::EntryType_t<EntryDef_>{val});
}


//-----------------------------------------------------------------------------

template <typename EntryDef_>
struct SetMiscKey : DynSetter {
    SetMiscKey(std::reference_wrapper<const EntryDef_> e, dm::EntryType_t<EntryDef_> v) :
        entryDef_{std::move(e)}, value{std::move(v)} {}
    std::reference_wrapper<const EntryDef_> entryDef_;
    dm::EntryType_t<EntryDef_> value;

    void set(dm::FullMarsRecord& marsVals, dm::MiscRecord& miscVals, MappingResult&) const override {
        entryDef_.get().get(miscVals) = value;
    }
    void print(util::PrintStream& ps) const override { ps << "SetMiscKey(" << value << ")"; }
};

template <typename EntryDef_>
SetMiscKey<EntryDef_> setMiscKey(const EntryDef_& entryDef,
                                 dm::EntryType_t<EntryDef_> val = dm::EntryType_t<EntryDef_>{}) {
    entryDef.applyDefaults(val);
    return SetMiscKey<EntryDef_>{std::cref(entryDef), std::move(val)};
}
template <typename EntryDef_>
SetMiscKey<EntryDef_> setMiscKey(const EntryDef_& entryDef, dm::EntryValueType_t<EntryDef_> val) {
    return setMiscKey(entryDef, dm::EntryType_t<EntryDef_>{val});
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


template <typename EntryDef_>
struct Print<mars2mars::rules::SetMarsKey<EntryDef_>> {
    static void print(PrintStream& ps, const mars2mars::rules::SetMarsKey<EntryDef_>& r) { r.print(ps); };
};

template <typename EntryDef_>
struct Print<mars2mars::rules::SetMiscKey<EntryDef_>> {
    static void print(PrintStream& ps, const mars2mars::rules::SetMiscKey<EntryDef_>& r) { r.print(ps); };
};

template <>
struct Print<mars2mars::rules::SetAll> {
    static void print(PrintStream& ps, const mars2mars::rules::SetAll& r) { r.print(ps); };
};


}  // namespace multio::util

