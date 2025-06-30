/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Oct 2025

#pragma once


#include "multio/action/encode-mtg2/Options.h"
#include "multio/action/encode-mtg2/multiom/MultIOMDict.h"
#include "multio/action/encode-mtg2/multiom/MultIOMRawEncoder.h"
#include "multio/action/encode-mtg2/multiom/MultIOMRules.h"
#include "multio/action/encode/GribEncoder.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/util/PrehashedKey.h"


namespace multio::action {

using PrehashedMarsKeys = util::PrehashedKey<datamod::EncoderCacheMarsKeyValueSet>;

class EncoderCache {
public:
    EncoderCache(const EncodeMtg2Conf& opts);
    EncoderCache(const EncodeMtg2Conf& conf, MultIOMDict&& options);
    

    
    std::unique_ptr<util::MioGribHandle> getSample(const datamod::MarsKeyValueSet& marsKeys, const MultIOMDict& marsDict, const MultIOMDict& parDict, const MultIOMDict& geoDict);
    

private:
    struct CacheEntry {
        EncoderInfo info;
        MultIOMRawEncoder encoder;
        std::unique_ptr<util::MioGribHandle> preparedSample; 
    };
    
    CacheEntry& makeOrGetEntry(const datamod::MarsKeyValueSet& marsKeys, const MultIOMDict& marsDict, const MultIOMDict& parDict, const MultIOMDict& geoDict);
    
    std::reference_wrapper<const EncodeMtg2Conf> conf_;
    MultIOMDict options_;
    MultIOMRules rules_;
    std::unique_ptr<util::MioGribHandle> baseSample_;
    std::unordered_map<PrehashedMarsKeys, CacheEntry> cache_{};
};


//---------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action
