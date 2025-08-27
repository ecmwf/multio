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


#include "multio/action/ChainedAction.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Print.h"
#include "multio/mars2grib/api/RawAPI.h"


namespace multio::action {

namespace dm = multio::datamod;

//-----------------------------------------------------------------------------

constexpr auto Cached
    = dm::EntryDef<bool>{"cached"}.withDefault(false).withAccessor([](auto&& v) { return &v.cached; });
    
constexpr auto GeoFromAtlas
    = dm::EntryDef<bool>{"geo-from-atlas"}.withDefault(false).withAccessor([](auto&& v) { return &v.geoFromAtlas; });


struct EncodeMtg2Options {
    dm::EntryType_t<decltype(Cached)> cached;
    dm::EntryType_t<decltype(GeoFromAtlas)> geoFromAtlas;

    static constexpr std::string_view record_name_ = "encode-mtg2";
    static constexpr auto record_entries_ = std::make_tuple(Cached, GeoFromAtlas);
};

//-----------------------------------------------------------------------------

class EncodeMtg2 : public ChainedAction {
public:
    explicit EncodeMtg2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    // Internal constructor delegate with prepared configuration for specific
    // encoder
    explicit EncodeMtg2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encoderConf);

    void print(std::ostream& os) const override;

    // TODO pgeier this option will be renamed and the action should get it own struct with parsing capabilities again
    EncodeMtg2Options opts_;
    mars2grib::Mars2GribRaw mars2grib_;
};


//---------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action

//-----------------------------------------------------------------------------

namespace multio::util {
template <>
struct Print<multio::action::EncodeMtg2Options> : datamod::PrintRecord {};
}  // namespace multio::util

//-----------------------------------------------------------------------------
