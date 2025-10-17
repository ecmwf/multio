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

#include "multio/util/Print.h"

#include <sstream>

namespace multio::mars2grib {

namespace dm = multio::datamod;

namespace {

std::unique_ptr<metkit::codes::CodesHandle> defaultSample() {
    // Basically GRIB2.tmpl from eccodes but without local section
    static const std::vector<unsigned char> data_{
        {0x47, 0x52, 0x49, 0x42, 0xff, 0xff, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb3, 0x00, 0x00,
         0x00, 0x15, 0x01, 0x00, 0x62, 0x00, 0x00, 0x04, 0x00, 0x01, 0x07, 0xd7, 0x03, 0x17, 0x0c, 0x00, 0x00, 0x00,
         0x02, 0x00, 0x00, 0x00, 0x48, 0x03, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x28, 0x06, 0xff, 0xff,
         0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x80, 0x00,
         0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0x05, 0x3c, 0xb1, 0xf7, 0x00, 0x00, 0x00,
         0x00, 0x20, 0x85, 0x3c, 0xb1, 0xf7, 0x15, 0x4a, 0x3f, 0xac, 0x00, 0x2a, 0xec, 0x48, 0x00, 0x00, 0x00, 0x20,
         0x00, 0x00, 0x00, 0x00, 0x22, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x80, 0x00, 0x00, 0x00,
         0x01, 0x00, 0x00, 0x00, 0x00, 0x64, 0x00, 0x00, 0x01, 0x86, 0xa0, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00,
         0x00, 0x00, 0x15, 0x05, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
         0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 0xff, 0x00, 0x00, 0x00, 0x05, 0x07, 0x37, 0x37, 0x37, 0x37}};

    return metkit::codes::codesHandleFromMessage(data_);
}

std::unique_ptr<metkit::codes::CodesHandle> prepareSample(std::unique_ptr<metkit::codes::CodesHandle> sample,
                                                          const dm::FullMarsRecord& marsKeys) {

    switch (marsKeys.repres.get()) {
        case dm::Repres::SH: {
            sample->set("numberOfDataPoints", 6);
            sample->set("numberOfValues", 6);
            sample->set("bitsPerValue", 16);
            sample->set("typeOfFirstFixedSurface", 105);
            sample->set("scaleFactorOfFirstFixedSurface", 0);
            sample->set("scaledValueOfFirstFixedSurface", 0);
            sample->set("gridDefinitionTemplateNumber", 50);
            sample->set("J", 1);
            sample->set("K", 1);
            sample->set("M", 1);
            sample->set("spectralType", 1);
            sample->set("spectralMode", 1);
            sample->set("dataRepresentationTemplateNumber", 51);
            return sample;
        }
        case dm::Repres::GG: {
            // sample->set("gridType", std::string("reduced_gg"));
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

    auto sample = defaultSample();

    // Prepare sample
    sample = prepareSample(std::move(sample), marsKeys);
    sample = encoder.allocateAndPreset(std::move(sample), mars, misc, geo);


    // Move encoder and prepared sample to cache
    return cache_
        .emplace(std::move(cacheKeySet), CacheEntry{std::move(sections), std::move(encoder), std::move(sample)})
        .first->second;
}


std::unique_ptr<metkit::codes::CodesHandle> EncoderCache::getHandle(const dm::FullMarsRecord& marsKeys,
                                                                    const MultIOMDict& mars, const MultIOMDict& misc,
                                                                    const MultIOMDict& geo) {
    CacheEntry& entry = makeOrGetEntry(marsKeys, mars, misc, geo);
    return entry.encoder.runtime(entry.preparedSample->clone(), mars, misc, geo);
}

std::unique_ptr<metkit::codes::CodesHandle> EncoderCache::getHandle(const dm::FullMarsRecord& marsKeys,
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
            ret->set("shapeOfTheEarth", 6);
        }
        // TODO pgeier this is a fix that needs to be moved to the section setters
        if (miscKeys.bitmapPresent.isSet()) {
            ret->set("bitmapPresent", miscKeys.bitmapPresent.get());
        }
        if (miscKeys.missingValue.isSet()) {
            ret->set("missingValue", miscKeys.missingValue.get());
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

std::unique_ptr<metkit::codes::CodesHandle> EncoderCache::getHandle(const dm::FullMarsRecord& marsKeys,
                                                                    const dm::MiscRecord& miscKeys) {
    auto geo = makeUnscopedGeometry(marsKeys);
    return getHandle(marsKeys, miscKeys, geo);
}


}  // namespace multio::mars2grib
