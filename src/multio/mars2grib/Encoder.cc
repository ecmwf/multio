/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/mars2grib/Encoder.h"
#include "multio/datamod/AtlasGeo.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"
#include "multio/datamod/core/TypeParserDumper.h"
#include "multio/datamod/types/GridType.h"
#include "multio/mars2grib/Grib2Layout.h"
#include "multio/mars2grib/LegacyEncoderConf.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/Rules.h"
#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/grib2/Geometry.h"
#include "multio/mars2grib/grib2/Time.h"
#include "multio/mars2grib/grib2/Utils.h"
#include "multio/mars2grib/multiom/MultIOMDict.h"

#include "multio/util/Print.h"

#include "metkit/codes/api/CodesAPI.h"

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


void testReadback(metkit::codes::CodesHandle& sample, const Grib2Layout& layout, const dm::Geometry& geo) {
    // TODO(pgeier) localTablesVersion not always matching after cloning
    // grib2::testReadback(sample, layout.structure);

    // TODO(pgeier) truncateDegree is changing after cloning
    std::visit([&](const auto& g) { grib2::testReadback(sample, g); }, geo);

    if (layout.initForecastTime) {
        grib2::testReadback(sample, *layout.initForecastTime);
    }
    if (layout.vertical) {
        grib2::testReadback(sample, *layout.vertical);
    }
    if (layout.satellite) {
        grib2::testReadback(sample, *layout.satellite);
    }
    if (layout.dirFreqArrays) {
        grib2::testReadback(sample, *layout.dirFreqArrays);
    }


    // TODO(pgeier) enable once implemented
    // if (layout.dirFreqMars) {
    //     grib2::testReadback(sample, *layout.dirFreqMars);
    // }
    // if (layout.pointInTime) {
    //     grib2::testReadback(sample, *layout.pointInTime);
    // }
    // if (layout.timeRange) {
    //     grib2::testReadback(sample, *layout.timeRange);
    // }
}

void prepareSample(metkit::codes::CodesHandle& sample, const dm::FullMarsRecord& marsKeys) {
    if (marsKeys.truncation.isSet()) {
        sample.set("numberOfDataPoints", 6);
        sample.set("numberOfValues", 6);
        sample.set("bitsPerValue", 16);
        sample.set("typeOfFirstFixedSurface", 105);
        sample.set("scaleFactorOfFirstFixedSurface", 0);
        sample.set("scaledValueOfFirstFixedSurface", 0);
        sample.set("gridDefinitionTemplateNumber", 50);
        sample.set("J", 1);
        sample.set("K", 1);
        sample.set("M", 1);
        sample.set("spectralType", 1);
        sample.set("spectralMode", 1);
        sample.set("dataRepresentationTemplateNumber", 51);
    }
}


void allocateSample(metkit::codes::CodesHandle& sample, const Grib2Layout& layout, const dm::Geometry& geo,
                    const EncoderOptions& opts) {
    // Set grib structure
    grib2::writeKeys(layout.structure, sample, opts.enableReadbackTest);

    // Set Geometry
    grib2::writeGeometry(geo, sample, opts.enableReadbackTest);

    // Initial time keys
    if (layout.initForecastTime) {
        grib2::writeKeys(*layout.initForecastTime, sample, opts.enableReadbackTest);
    }

    // Set verticals
    if (layout.vertical) {
        grib2::writeKeys(*layout.vertical, sample, opts.enableReadbackTest);
    }

    // TODO(pgeier) to be moved to preset
    if (layout.satellite) {
        grib2::writeKeys(*layout.satellite, sample, opts.enableReadbackTest);
    }

    if (layout.dirFreqArrays) {
        grib2::writeKeys(*layout.dirFreqArrays, sample, opts.enableReadbackTest);
    }
}

void presetSample(metkit::codes::CodesHandle& sample, Grib2Layout& layout, const EncoderOptions& opts) {}

void finalizeSample(metkit::codes::CodesHandle& sample, Grib2Layout& layout, const EncoderOptions& opts) {
    if (layout.pointInTime) {
        // TODO(pgeier)
    }
    if (layout.timeRange) {
        // TODO(pgeier)
    }
}

}  // namespace


Encoder::Encoder(EncoderOptions opts) :
    opts_{opts},
    cache_{opts.enableCache ? std::optional<Encoder::Cache>{Encoder::Cache{}} : std::optional<Encoder::Cache>{}} {};

PreparedEncoder prepareEncoder(const dm::FullMarsRecord& marsKeys, const dm::MiscRecord& miscKeys,
                               const MultIOMDict& mars, const MultIOMDict& misc, const dm::Geometry& geo,
                               const EncoderOptions& opts) {
    // Searching for rule...
    auto [sections, layout] = rules::buildEncoderConf(marsKeys, miscKeys);
    auto exportedConf = dm::dumpRecord<eckit::LocalConfiguration>(sections);

    MultIOMRawEncoder encoder{exportedConf};

    auto sample = defaultSample();

    // Set defaultTablesVersion
    if (!layout.structure.tablesVersion.isSet()) {
        if (miscKeys.tablesVersion.isSet()) {
            layout.structure.tablesVersion.set(miscKeys.tablesVersion.get());
        }
        else {
            layout.structure.tablesVersion.set(sample->getLong("tablesVersionLatest"));
        }
    }

    layout.structure.productDefinitionTemplateNumber.set(
        rules::InferPdt<>{}.inferProductDefinitionTemplateNumber(layout.pdtCat));

    // Check grid consistency
    auto geoGridType = dm::gridTypeFromGeometry(geo);
    if (!layout.structure.gridType.isSet()) {
        std::ostringstream oss;
        util::PrintStream ps(oss);
        ps << "No gridType could be bemapped from MARS - expected " << geoGridType;
        throw Mars2GribException(oss.str(), Here());
    }
    if (layout.structure.gridType.get() != geoGridType) {
        std::ostringstream oss;
        util::PrintStream ps(oss);
        ps << "The gridType infered from MARS is different to the geometry passed: " << layout.structure.gridType.get()
           << " != " << geoGridType << std::endl;
        ps << "Mars: " << std::endl;
        {
            util::IndentGuard g(ps);
            ps << marsKeys << std::endl;
            ;
        }
        throw Mars2GribException(oss.str(), Here());
    }
    datamod::applyRecordDefaults(layout.structure);
    datamod::validateRecord(layout.structure);

    // Prepare sample
    prepareSample(*sample.get(), marsKeys);
    sample = sample->clone();  // Safe reload

    // Migrated calls
    allocateSample(*sample.get(), layout, geo, opts);
    sample = sample->clone();  // Safe reload

    // presetSample(*sample.get(), layout, opts);
    // sample = sample->duplicate(); // Safe reload

    // TODO(pgeier) Remove - internally not used anymore
    MultIOMDict geom{MultIOMDictKind::ReducedGG};
    // TODO(pgeier) legocy calls
    sample = encoder.allocateAndPreset(std::move(sample), mars, misc, geom);

    if (opts.enableReadbackTest) {
        testReadback(*sample.get(), layout, geo);
    }

    return PreparedEncoder({std::move(sections), std::move(encoder), std::move(sample)});
}


PreparedEncoder& Encoder::makeOrGetEntry(const dm::FullMarsRecord& marsKeys, const dm::MiscRecord& miscKeys,
                                         const MultIOMDict& mars, const MultIOMDict& misc, const dm::Geometry& geo) {
    // Select caching keys and prehash
    PrehashedMarsKeys cacheKeySet = dm::readRecord<MarsCacheRecord>(marsKeys);

    // Search and return if entry already exists
    if (auto search = cache_->find(cacheKeySet); search != cache_->end()) {
        return search->second;
    }

    // Move encoder and prepared sample to cache
    return cache_->emplace(std::move(cacheKeySet), prepareEncoder(marsKeys, miscKeys, mars, misc, geo, opts_))
        .first->second;
}


std::unique_ptr<metkit::codes::CodesHandle> Encoder::getHandle(const dm::FullMarsRecord& marsKeys,
                                                               const dm::MiscRecord& miscKeys, const MultIOMDict& mars,
                                                               const MultIOMDict& misc, const dm::Geometry& geo) {
    if (cache_) {
        PreparedEncoder& entry = makeOrGetEntry(marsKeys, miscKeys, mars, misc, geo);
        // TODO(pgeier) remove... fake
        MultIOMDict geom{MultIOMDictKind::ReducedGG};
        return entry.encoder.runtime(entry.preparedSample->clone(), mars, misc, geom);
    }
    else {
        PreparedEncoder entry = prepareEncoder(marsKeys, miscKeys, mars, misc, geo, opts_);
        // TODO(pgeier) remove... fake
        MultIOMDict geom{MultIOMDictKind::ReducedGG};
        return entry.encoder.runtime(std::move(entry.preparedSample), mars, misc, geom);
    }
}

std::unique_ptr<metkit::codes::CodesHandle> Encoder::getHandle(const dm::FullMarsRecord& marsKeys,
                                                               const dm::MiscRecord& miscKeys,
                                                               const dm::Geometry& geoKeys) {
    try {
        MultIOMDict mars{MultIOMDictKind::MARS};
        MultIOMDict misc{MultIOMDictKind::Parametrization};

        dm::dumpRecord(marsKeys, mars);
        dm::dumpRecord(miscKeys, misc);

        auto ret = getHandle(marsKeys, miscKeys, mars, misc, geoKeys);

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
        ps << "Failure in Encoder::getHandle" << std::endl;
        {
            util::IndentGuard gout(ps);

            ps << "Mars: " << marsKeys << std::endl;
            ps << "Misc: " << miscKeys << std::endl;
            ps << "Geo: " << geoKeys << std::endl;
        }

        std::throw_with_nested(mars2grib::Mars2GribException(oss.str(), Here()));
    }
}

std::unique_ptr<metkit::codes::CodesHandle> Encoder::getHandle(const dm::FullMarsRecord& marsKeys,
                                                               const dm::MiscRecord& miscKeys) {
    auto geo = makeUnscopedGeometry(marsKeys);
    return getHandle(marsKeys, miscKeys, geo);
}


}  // namespace multio::mars2grib
