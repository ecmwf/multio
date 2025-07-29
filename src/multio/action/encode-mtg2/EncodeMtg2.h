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
#include "multio/mars2grib/EncoderCache.h"
#include "multio/mars2grib/Options.h"


namespace multio::action {

class EncodeMtg2 : public ChainedAction {
public:
    explicit EncodeMtg2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    // Internal constructor delegate with prepared configuration for specific
    // encoder
    explicit EncodeMtg2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encoderConf);

    void print(std::ostream& os) const override;

    // TODO this option will be renamed and the action should get it own struct with parsing capabilities again
    mars2grib::EncodeMtg2Conf conf_;
    mars2grib::EncoderCache cache_;
};


//---------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action
