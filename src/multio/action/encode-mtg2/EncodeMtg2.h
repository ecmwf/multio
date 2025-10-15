/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once


#include "multio/action/ChainedAction.h"
#include "multio/mars2grib/api/RawAPI.h"
#include "multio/util/config/Parser.h"


namespace multio::action {

namespace cf = multio::util::config;

//----------------------------------------------------------------------------------------------------------------------

struct EncodeMtg2Options {
    bool cached = true;
    bool geoFromAtlas = false;
    bool readbackTest = false;

    static constexpr auto fields_
        = std::make_tuple(cf::optionalEntry("cached", &EncodeMtg2Options::cached),
                          cf::optionalEntry("geo-from-atlas", &EncodeMtg2Options::geoFromAtlas),
                          cf::optionalEntry("readback-test", &EncodeMtg2Options::readbackTest));
};

//----------------------------------------------------------------------------------------------------------------------

class EncodeMtg2 : public ChainedAction {
public:
    explicit EncodeMtg2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    // Internal constructor delegate with prepared configuration for specific
    // encoder
    explicit EncodeMtg2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encoderConf);

    void print(std::ostream& os) const override;

    // TODO pgeier this option will be renamed and the action should get it own struct with parsing capabilities again
    EncodeMtg2Options opts_;
    mars2grib::Mars2GribRaw mars2grib_;
};


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action
