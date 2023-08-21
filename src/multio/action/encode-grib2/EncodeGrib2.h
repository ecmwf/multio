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

/// @date Oct 2019

#pragma once

#include "multio/action/ChainedAction.h"
#include "multio/action/encode/GridDownloader.h"
#include "multio/action/encode/MioGribHandle.h"

#include <optional>

namespace multio::action {

class EncodeGrib2 : public ChainedAction {
public:
    explicit EncodeGrib2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    // Internal constructor delegate with prepared configuration for specific
    // encoder
    explicit EncodeGrib2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encoderConf);

    void print(std::ostream& os) const override;

    message::Message setFieldValues(const message::Message& msg);
    message::Message setFieldValues(const double* values, size_t count);
    message::Message setFieldValues(const float* values, size_t count);


    message::Message encodeField(const message::Message& msg, const std::optional<std::string>& gridUID) const;

    const MioGribHandle template_;
    std::optional<eckit::LocalConfiguration> overwrite_;
    std::unique_ptr<MioGribHandle> encoder_;
    const std::unique_ptr<GridDownloader> gridDownloader_ = nullptr;
};

//=====================================================================================================================

class EncodeGrib2Exception : public eckit::Exception {
public:
    EncodeGrib2Exception(const std::string& reason, const eckit::CodeLocation& location = eckit::CodeLocation());
};

//=====================================================================================================================


}  // namespace multio::action
