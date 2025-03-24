/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/action/encode-mtg2/EncoderCache.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/action/encode-mtg2/Options.h"
#include "multio/action/encode-mtg2/multiom/MultIOMDict.h"
#include "multio/action/encode-mtg2/multiom/MultIOMRules.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/util/MioGribHandle.h"

namespace multio::action {

namespace {

std::unique_ptr<util::MioGribHandle> loadSample(const std::string& sample) {
    codes_handle* h = codes_handle_new_from_samples(nullptr, sample.c_str());
    if (h == nullptr) {
        throw EncodeMtg2Exception(std::string("Unable to load eccodes sample: ") + sample, Here());
    }
    return std::make_unique<util::MioGribHandle>(codes_handle_new_from_samples(nullptr, sample.c_str()));
}

}  // namespace


EncoderCache::EncoderCache(const EncodeMtg2Conf& opts) : EncoderCache(opts, MultIOMDict::makeOptions(opts)) {}


EncoderCache::EncoderCache(const EncodeMtg2Conf& conf, MultIOMDict&& options) :
    conf_{conf}, options_{std::move(options)}, rules_{options_, conf_}, baseSample_{loadSample("sample")} {}


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
    auto encoderConf = rules_.search(mars);
    MultIOMRawEncoder encoder{options_, datamod::key<EncoderDef::Conf>(encoderConf)};

    // Load custom sample or use default sample
    const auto& sampleName = datamod::key<EncoderDef::Sample>(encoderConf);
    auto sample = sampleName.has() ? loadSample(sampleName.get()) : baseSample_->duplicate();

    // Prepare sample
    sample = encoder.prepare(std::move(sample), mars, par, geo);

    // Move encoder and prepared sample to cache
    return cache_
        .emplace(std::move(cacheKeySet), CacheEntry{std::move(encoderConf), std::move(encoder), std::move(sample)})
        .first->second;
}


std::unique_ptr<util::MioGribHandle> EncoderCache::getSample(const datamod::MarsKeyValueSet& marsKeys,
                                                             const MultIOMDict& mars, const MultIOMDict& par,
                                                             const MultIOMDict& geo) {
    CacheEntry& entry = makeOrGetEntry(marsKeys, mars, par, geo);
    return entry.encoder.runtime(entry.preparedSample->duplicate(), mars, par, geo);
}


}  // namespace multio::action
