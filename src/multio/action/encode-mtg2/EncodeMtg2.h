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

#include "metkit/mars2grib/api/Mars2Grib.h"
#include "multio/action/ChainedAction.h"


namespace multio::action::encode_mtg2 {

//----------------------------------------------------------------------------------------------------------------------

class EncodeMtg2 : public ChainedAction {
public:
    explicit EncodeMtg2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;

    metkit::mars2grib::Mars2Grib encoder_;
};


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action::encode_mtg2
