/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/action/encode-mtg2/multiom/MultIOMRules.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/action/encode-mtg2/Options.h"

#include <iostream>

// #include "multio/util/MioGribHandle.h"

namespace multio::action {


MultIOMRules::MultIOMRules(const MultIOMDict& options, const std::string& fname) {
    void* handle = NULL;
    if (multio_grib2_rules_open(options.get(), &handle, fname.data(), fname.length()) != 0) {
        throw EncodeMtg2Exception(std::string("Can not open rules ") + fname, Here());
    }

    rules_.reset(static_cast<ForeignRulesType*>(handle));
}

MultIOMRules::MultIOMRules(const MultIOMDict& options, const EncodeMtg2Conf& conf) :
    MultIOMRules(options, datamod::key<EncodeMtg2Def::EncodingRules>(conf).get()) {}

void* MultIOMRules::get() const {
    return static_cast<void*>(rules_.get());
}

EncoderConf MultIOMRules::search(const MultIOMDict& mars) {
    char* cRulePath = nullptr;
    if (multio_grib2_rules_search(rules_.get(), mars.get(), &cRulePath) != 0) {
        throw EncodeMtg2Exception(std::string("Failed searching for rule"), Here());
    }
    std::string rulePath{cRulePath};

    // Convert the char* result to a std::string
    eckit::LocalConfiguration conf{eckit::YAMLConfiguration{rulePath}};

    return datamod::readByValue(EncoderDefKeySet{}, conf);
}


}  // namespace multio::action
