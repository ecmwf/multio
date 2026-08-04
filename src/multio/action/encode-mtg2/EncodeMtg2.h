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

#include <memory>
#include <optional>

#include "metkit/mars2grib/api/Mars2Grib.h"
#include "multio/action/ChainedAction.h"
#include "multio/datamod/MarsCachedKeys.h"
#include "multio/util/config/Parser.h"


namespace multio::action::encode_mtg2 {

namespace cf = multio::util::config;

//----------------------------------------------------------------------------------------------------------------------

struct EncodeMtg2Options {
    bool cached = false;

    static constexpr auto fields_ = std::make_tuple(cf::optionalEntry("cached", &EncodeMtg2Options::cached));
};

//----------------------------------------------------------------------------------------------------------------------

using PrehashedMarsKeys = util::PrehashedKey<datamod::MarsCacheRecord>;
using Cache = std::unordered_map<PrehashedMarsKeys, metkit::mars2grib::Mars2Grib::CacheEntryPtr>;

//----------------------------------------------------------------------------------------------------------------------

class EncodeMtg2 : public ChainedAction {
public:
    explicit EncodeMtg2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;

    // TODO pgeier this option will be renamed and the action should get it own struct with parsing capabilities again
    EncodeMtg2Options opts_;
    std::unique_ptr<metkit::mars2grib::Mars2Grib> encoder_;

    std::optional<Cache> cache_;
};


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action::encode_mtg2
