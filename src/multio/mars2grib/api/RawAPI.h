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
#include "multio/mars2grib/EncoderCache.h"

namespace multio::mars2grib {

//---------------------------------------------------------------------------------------------------------------------

struct RawOptions {
    bool cached = true;
};

//---------------------------------------------------------------------------------------------------------------------

class Mars2GribRaw {
public:
    Mars2GribRaw(RawOptions options = RawOptions{});  // Only possible constructor

    Mars2GribRaw(const Mars2GribRaw&) = delete;
    Mars2GribRaw(Mars2GribRaw&&) = default;


    Mars2GribRaw& operator=(const Mars2GribRaw&) = delete;
    Mars2GribRaw& operator=(Mars2GribRaw&&) = default;

    ~Mars2GribRaw() = default;

    std::unique_ptr<util::MioGribHandle> getHandle(const dm::MarsRecord& marsKeys, const dm::MiscRecord& miscKeys,
                                                   const dm::Geometry& geoKeys);

private:
    EncoderCache cache_;
};

}  // namespace multio::mars2grib
