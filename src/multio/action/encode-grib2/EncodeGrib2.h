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

/// @date August 2023

#pragma once

#include "multio/action/ChainedAction.h"
#include "multio/action/encode-grib2/SampleManager.h"

namespace multio::action {

// TODO handle local definition number

class EncodeGrib2 : public ChainedAction {
public:
    explicit EncodeGrib2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

protected:
    eckit::Buffer encodeSample(MioGribHandle& sample) const;

    message::Message encodeMessageWithData(MioGribHandle& sample, const message::Message& inMsg) const;
    message::Message encodeMessageWithoutData(MioGribHandle& sample, const message::Message& inMsg) const;

    // Transfering general values (section 1), mars keys and product information
    void transferRelevantValues(const encodeGrib2::SampleKey& sampleKey, const message::Metadata& from,
                                MioGribHandle& to) const;

    // Transfering general values (section 1) and mars keys - no product information
    void transferRelevantValues(const message::Metadata& from, MioGribHandle& to) const;


    // Internal constructor delegate with prepared configuration for specific
    // encoder
    explicit EncodeGrib2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encoderConf);

    void print(std::ostream& os) const override;

    encodeGrib2::SampleManager sampleManager_;
    std::optional<eckit::LocalConfiguration> overwrite_;
};

//---------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action
