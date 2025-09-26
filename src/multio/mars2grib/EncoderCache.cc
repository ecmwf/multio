/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/mars2grib/EncoderCache.h"
#include "multio/datamod/AtlasGeo.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/Rules.h"
#include "multio/mars2grib/multiom/MultIOMDict.h"
#include "multio/util/MioGribHandle.h"

#include "multio/util/Print.h"

#include <sstream>

namespace multio::mars2grib {

namespace dm = multio::datamod;

namespace {

std::unique_ptr<util::MioGribHandle> prepareSample(std::unique_ptr<util::MioGribHandle> sample,
                                                   const dm::FullMarsRecord& marsKeys) {

    switch (marsKeys.repres.get()) {
        case dm::Repres::SH: {
            sample->setValue("numberOfDataPoints", 6);
            sample->setValue("numberOfValues", 6);
            sample->setValue("bitsPerValue", 16);
            sample->setValue("typeOfFirstFixedSurface", 105);
            sample->setValue("scaleFactorOfFirstFixedSurface", 0);
            sample->setValue("scaledValueOfFirstFixedSurface", 0);
            sample->setValue("gridDefinitionTemplateNumber", 50);
            sample->setValue("J", 1);
            sample->setValue("K", 1);
            sample->setValue("M", 1);
            sample->setValue("spectralType", 1);
            sample->setValue("spectralMode", 1);
            sample->setValue("dataRepresentationTemplateNumber", 51);
            return sample;
        }
        case dm::Repres::GG: {
            // sample->setValue("gridType", std::string("reduced_gg"));
            return sample;
        }
        default: {
            return sample;
        }
    }
}

}  // namespace


EncoderCache::CacheEntry& EncoderCache::makeOrGetEntry(const dm::FullMarsRecord& marsKeys, const MultIOMDict& mars,
                                                       const MultIOMDict& misc, const MultIOMDict& geo) {
    // Select caching keys and prehash
    PrehashedMarsKeys cacheKeySet = dm::readRecord<MarsCacheRecord>(marsKeys);

    // Search and return if entry already exists
    if (auto search = cache_.find(cacheKeySet); search != cache_.end()) {
        return search->second;
    }

    // Otherwise prepare a new entry

    // Searching for rule...

    SectionsConf sections = rules::buildEncoderConf(marsKeys);
    auto exportedConf = dm::dumpRecord<eckit::LocalConfiguration>(sections);

    MultIOMRawEncoder encoder{exportedConf};

    auto sample = util::MioGribHandle::makeDefault();

    // Prepare sample
    sample = prepareSample(std::move(sample), marsKeys);
    sample = encoder.allocateAndPreset(std::move(sample), mars, misc, geo);


    // Move encoder and prepared sample to cache
    return cache_
        .emplace(std::move(cacheKeySet), CacheEntry{std::move(sections), std::move(encoder), std::move(sample)})
        .first->second;
}


std::unique_ptr<util::MioGribHandle> EncoderCache::getHandle(const dm::FullMarsRecord& marsKeys,
                                                             const MultIOMDict& mars, const MultIOMDict& misc,
                                                             const MultIOMDict& geo) {
    CacheEntry& entry = makeOrGetEntry(marsKeys, mars, misc, geo);
    return entry.encoder.runtime(entry.preparedSample->duplicate(), mars, misc, geo);
}

std::unique_ptr<util::MioGribHandle> EncoderCache::getHandle(const dm::FullMarsRecord& marsKeys,
                                                             const dm::MiscRecord& miscKeys,
                                                             const dm::Geometry& geoKeys) {
    try {
        MultIOMDict mars{MultIOMDictKind::MARS};
        MultIOMDict misc{MultIOMDictKind::Parametrization};

        dm::dumpRecord(marsKeys, mars);
        dm::dumpRecord(miscKeys, misc);

        // Setup MultIOM dict
        MultIOMDict geom{([&]() {
            switch (marsKeys.repres.get()) {
                case dm::Repres::GG:
                    return MultIOMDictKind::ReducedGG;
                case dm::Repres::HEALPix:
                    return MultIOMDictKind::HEALPix;
                case dm::Repres::LL:
                    return MultIOMDictKind::RegularLL;
                case dm::Repres::SH:
                    return MultIOMDictKind::SH;
            }
            throw Mars2GribException("unkown repres", Here());
        })()};

        std::visit([&](auto& specificGeoKeys) { dm::dumpRecord(specificGeoKeys, geom); }, geoKeys);

        // return getHandle(marsKeys, mars, misc, geom);
        auto ret = getHandle(marsKeys, mars, misc, geom);

        // TODO pgeier fix that needs to be expressed in GeoGG once MULTIOM is gone
        if (marsKeys.repres.get() == dm::Repres::GG) {
            ret->setValue("shapeOfTheEarth", 6);
        }
        // TODO pgeier this is a fix that needs to be moved to the section setters
        if (miscKeys.bitmapPresent.isSet()) {
            ret->setValue("bitmapPresent", miscKeys.bitmapPresent.get());
        }
        if (miscKeys.missingValue.isSet()) {
            ret->setValue("missingValue", miscKeys.missingValue.get());
        }
        return ret;
    }
    catch (...) {
        std::ostringstream oss;
        util::PrintStream ps(oss);
        ps.repres(util::PrintRepres::Compact);
        ps << "Failure in EncoderCache::getHandle" << std::endl;
        {
            util::IndentGuard gout(ps);

            ps << "Mars: " << marsKeys << std::endl;
            ps << "Misc: " << miscKeys << std::endl;
            ps << "Geo: " << geoKeys << std::endl;
        }

        std::throw_with_nested(mars2grib::Mars2GribException(oss.str(), Here()));
    }
}

std::unique_ptr<util::MioGribHandle> EncoderCache::getHandle(const dm::FullMarsRecord& marsKeys,
                                                             const dm::MiscRecord& miscKeys) {
    auto geo = makeUnscopedGeometry(marsKeys);
    return getHandle(marsKeys, miscKeys, geo);
}


}  // namespace multio::mars2grib
