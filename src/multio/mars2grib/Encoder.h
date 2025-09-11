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
#include "multio/mars2grib/LegacyEncoderConf.h"
#include "multio/mars2grib/MarsCachedKeys.h"
#include "multio/mars2grib/multiom/MultIOMDict.h"
#include "multio/mars2grib/multiom/MultIOMRawEncoder.h"
#include "multio/util/PrehashedKey.h"

#include "metkit/codes/api/CodesAPI.h"


namespace multio::mars2grib {

namespace dm = multio::datamod;

struct PreparedEncoder {
    LegacySectionsConf sections;
    MultIOMRawEncoder encoder;
    std::unique_ptr<metkit::codes::CodesHandle> preparedSample;
};

PreparedEncoder prepareEncoder(const dm::FullMarsRecord& marsKeys, const MultIOMDict& marsDict,
                               const MultIOMDict& parDict, const dm::Geometry& geoDict);


using PrehashedMarsKeys = util::PrehashedKey<MarsCacheRecord>;

class Encoder {
public:
    Encoder(bool cached = true);
    Encoder(const Encoder&) = default;
    Encoder(Encoder&&) = default;
    Encoder& operator=(const Encoder&) = default;
    Encoder& operator=(Encoder&&) = default;

    // Try to infer geometry and then prepare a grib handle
    std::unique_ptr<metkit::codes::CodesHandle> getHandle(const dm::FullMarsRecord& marsKeys,
                                                          const dm::MiscRecord& miscKeys);
    // Perpare a grib handle
    std::unique_ptr<metkit::codes::CodesHandle> getHandle(const dm::FullMarsRecord& marsKeys,
                                                          const dm::MiscRecord& miscKeys, const dm::Geometry& geoKeys);

protected:
    // Prepare a grib handle with already mapped MultIOM dicts
    std::unique_ptr<metkit::codes::CodesHandle> getHandle(const dm::FullMarsRecord& marsKeys,
                                                          const dm::MiscRecord& miscKeys, const MultIOMDict& marsDict,
                                                          const MultIOMDict& parDict, const dm::Geometry& geoDict);

private:
    PreparedEncoder& makeOrGetEntry(const dm::FullMarsRecord& marsKeys, const dm::MiscRecord& miscKeys,
                                    const MultIOMDict& marsDict, const MultIOMDict& parDict,
                                    const dm::Geometry& geoDict);

    using Cache = std::unordered_map<PrehashedMarsKeys, PreparedEncoder>;
    std::optional<Cache> cache_{};
};


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::mars2grib
