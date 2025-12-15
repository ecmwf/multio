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


#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/mars2grib/EncoderConf.h"
#include "multio/mars2grib/MarsCachedKeys.h"
#include "multio/mars2grib/multiom/MultIOMDict.h"
#include "multio/mars2grib/multiom/MultIOMRawEncoder.h"
#include "multio/util/PrehashedKey.h"

#include "metkit/codes/api/CodesAPI.h"


namespace multio::mars2grib {

namespace dm = multio::datamod;

using PrehashedMarsKeys = util::PrehashedKey<MarsCacheRecord>;

class EncoderCache {
public:
    EncoderCache() = default;
    EncoderCache(const EncoderCache&) = default;
    EncoderCache(EncoderCache&&) = default;
    EncoderCache& operator=(const EncoderCache&) = default;
    EncoderCache& operator=(EncoderCache&&) = default;

    // Try to infer geometry and then prepare a grib handle
    std::unique_ptr<metkit::codes::CodesHandle> getHandle(const dm::FullMarsRecord& marsKeys,
                                                          const dm::MiscRecord& miscKeys);
    // Perpare a grib handle
    std::unique_ptr<metkit::codes::CodesHandle> getHandle(const dm::FullMarsRecord& marsKeys,
                                                          const dm::MiscRecord& miscKeys,
                                                          const dm::Geometry& geomKeys);

protected:
    // Prepare a grib handle with already mapped MultIOM dicts
    std::unique_ptr<metkit::codes::CodesHandle> getHandle(const dm::FullMarsRecord& marsKeys, const MultIOMDict& marsDict,
                                                          const dm::MiscRecord& miscKeys, const MultIOMDict& miscDict,
                                                          const dm::Geometry& geomKeys, const MultIOMDict& geomDict);

private:
    struct CacheEntry {
        SectionsConf sections;
        MultIOMRawEncoder encoder;
        std::unique_ptr<metkit::codes::CodesHandle> preparedSample;
    };

    CacheEntry& makeOrGetEntry(const dm::FullMarsRecord& marsKeys, const MultIOMDict& marsDict,
                               const dm::MiscRecord& miscKeys, const MultIOMDict& miscDict,
                               const dm::Geometry& geomKeys, const MultIOMDict& geomDict);

    std::unordered_map<PrehashedMarsKeys, CacheEntry> cache_{};
};


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::mars2grib
