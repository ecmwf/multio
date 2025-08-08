/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/mars2grib/EncoderCache.h"
#include <exception>
#include "multio/datamod/AtlasGeo.h"
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/KeyValueSet.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/Options.h"
#include "multio/mars2grib/Rules.h"
#include "multio/mars2grib/multiom/MultIOMDict.h"
#include "multio/util/MioGribHandle.h"

#include "eckit/filesystem/PathName.h"

#include <sstream>

namespace multio::mars2grib {

namespace {

std::unique_ptr<util::MioGribHandle> prepareSample(std::unique_ptr<util::MioGribHandle> sample,
                                                   const datamod::MarsKeyValueSet& marsKeys) {

    switch (datamod::key<datamod::MarsKeys::REPRES>(marsKeys).get()) {
        case datamod::Repres::SH: {
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
        case datamod::Repres::GG: {
            sample->setValue("gridType", std::string("reduced_gg"));
            return sample;
        }
        default: {
            return sample;
        }
    }
}

std::unique_ptr<util::MioGribHandle> loadSample(const EncodeMtg2Conf& conf, const std::string& sample) {
    using namespace datamod;
    const auto& samplesPath = key<EncodeMtg2Def::SamplesPath>(conf);
    return std::make_unique<util::MioGribHandle>(eckit::PathName{samplesPath.get() + std::string("/") + sample});
}

}  // namespace


EncoderCache::EncoderCache(const EncodeMtg2Conf& opts) : EncoderCache(opts, MultIOMDict::makeOptions(opts)) {}

EncoderCache::EncoderCache() :
    EncoderCache(([]() {
        EncodeMtg2Conf res{};
        datamod::alterAndValidate(res);
        return res;
    })()) {}


EncoderCache::EncoderCache(const EncodeMtg2Conf& conf, MultIOMDict&& options) :
    // conf_{conf}, options_{std::move(options)}, rules_{options_, conf_}, baseSample_{loadSample(conf_, "sample.tmpl")}
    // {}
    conf_{conf}, options_{std::move(options)}, baseSample_{loadSample(conf_, "sample.tmpl")} {}


EncoderCache::CacheEntry& EncoderCache::makeOrGetEntry(const datamod::MarsKeyValueSet& marsKeys,
                                                       const MultIOMDict& mars, const MultIOMDict& par,
                                                       const MultIOMDict& geo) {
    using namespace multio::datamod;
    // Select caching keys and prehash
    PrehashedMarsKeys cacheKeySet = read(EncoderCacheMarsKeySet{}, marsKeys);

    // Search and return if entry already exists
    if (auto search = cache_.find(cacheKeySet); search != cache_.end()) {
        return search->second;
    }

    // Otherwise prepare a new entry

    // Searching for rule...
    // auto encoderConf = rules_.search(mars);
    // auto exportedConf =
    // datamod::write<eckit::LocalConfiguration>(datamod::key<EncoderInfoDef::Sections>(encoderConf).get());

    EncoderSections sections = buildEncoderConf(marsKeys);
    auto exportedConf = datamod::write<eckit::LocalConfiguration>(sections);

    MultIOMRawEncoder encoder{options_, exportedConf};

    // Load custom sample or use default sample
    // const auto& sampleName = datamod::key<EncoderInfoDef::Sample>(encoderConf);
    // auto sample = sampleName.has() ? loadSample(sampleName.get()) : baseSample_->duplicate();
    auto sample = baseSample_->duplicate();

    // Prepare sample
    sample = prepareSample(std::move(sample), marsKeys);
    sample = encoder.allocateAndPreset(std::move(sample), mars, par, geo);

    // Move encoder and prepared sample to cache
    // return cache_
    //     .emplace(std::move(cacheKeySet), CacheEntry{std::move(encoderConf), std::move(encoder), std::move(sample)})
    //     .first->second;
    return cache_
        .emplace(std::move(cacheKeySet), CacheEntry{std::move(sections), std::move(encoder), std::move(sample)})
        .first->second;
}


std::unique_ptr<util::MioGribHandle> EncoderCache::getHandle(const datamod::MarsKeyValueSet& marsKeys,
                                                             const MultIOMDict& mars, const MultIOMDict& par,
                                                             const MultIOMDict& geo) {
    CacheEntry& entry = makeOrGetEntry(marsKeys, mars, par, geo);
    return entry.encoder.runtime(entry.preparedSample->duplicate(), mars, par, geo);
}

std::unique_ptr<util::MioGribHandle> EncoderCache::getHandle(const datamod::MarsKeyValueSet& marsKeys,
                                                             const datamod::MiscKeyValueSet& miscKeys,
                                                             const datamod::Geometry& geoKeys) {
    try {
        MultIOMDict mars{MultIOMDictKind::MARS};
        MultIOMDict misc{MultIOMDictKind::Parametrization};

        using namespace datamod;
        write(marsKeys, mars);
        write(miscKeys, misc);

        // Setup MultIOM dict
        const auto& repres = key<MarsKeys::REPRES>(marsKeys);
        MultIOMDict geom{([&]() {
            switch (repres.get()) {
                case Repres::GG:
                    return MultIOMDictKind::ReducedGG;
                case Repres::HEALPix:
                    return MultIOMDictKind::HEALPix;
                case Repres::LL:
                    return MultIOMDictKind::RegularLL;
                case Repres::SH:
                    return MultIOMDictKind::SH;
            }
            throw Mars2GribException("unkown repres", Here());
        })()};

        std::visit([&](auto& specificGeoKeys) { write(specificGeoKeys, geom); }, geoKeys);

        return getHandle(marsKeys, mars, misc, geom);
    }
    catch (...) {
        std::ostringstream oss;
        oss << "Failure in EncoderCache::getSample" << std::endl;
        oss << "Mars: ";
        util::print(oss, marsKeys);
        oss << std::endl << "Misc: ";
        util::print(oss, miscKeys);
        oss << std::endl << "Geo: ";
        util::print(oss, geoKeys);
        std::throw_with_nested(mars2grib::Mars2GribException(oss.str(), Here()));
    }
}

std::unique_ptr<util::MioGribHandle> EncoderCache::getHandle(const datamod::MarsKeyValueSet& marsKeys,
                                                             const datamod::MiscKeyValueSet& miscKeys) {
    auto geo = makeGeometry(marsKeys, true);
    return getHandle(marsKeys, miscKeys, geo);
}


}  // namespace multio::mars2grib
