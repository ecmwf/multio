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
#include "multio/mars2grib/Grib2Layout.h"
#include "multio/mars2grib/LegacyEncoderConf.h"


namespace multio::mars2grib::rules {

using Setter = std::function<void(const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf&, Grib2Layout&)>;

struct SetAll {
    std::vector<Setter> setters;

    void operator()(const dm::FullMarsRecord& mars, const dm::MiscRecord& misc, LegacySectionsConf& conf, Grib2Layout& g2l) const {
        for (const auto& setter : setters) {
            setter(mars, misc, conf, g2l);
        }
    }
};


template <typename... Setters>
auto setAll(Setters&&... setters) {
    SetAll res;
    (res.setters.emplace_back(std::forward<Setters>(setters)), ...);
    return res;
}

}  // namespace multio::mars2grib::rules
