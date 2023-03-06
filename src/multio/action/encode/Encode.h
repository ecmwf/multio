/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#pragma once

#include "GribEncoder.h"
#include "eckit/utils/Optional.h"
#include "multio/action/ChainedAction.h"

namespace multio {
namespace action {

class GridDownloader;

class Encode : public ChainedAction {
public:
    explicit Encode(const ConfigurationContext& confCtx);

    void executeImpl(message::Message msg) override;

private:
    // Internal constructor delegate with prepared configuration for specific
    // encoder
    explicit Encode(const ConfigurationContext& confCtx, ConfigurationContext&& encConfCtx);

    void print(std::ostream& os) const override;

    message::Message encodeField(const message::Message& msg) const;

    const std::string format_;
    eckit::Optional<eckit::LocalConfiguration> overwrite_;

    const std::unique_ptr<GribEncoder> encoder_ = nullptr;
    const std::unique_ptr<GridDownloader> gridDownloader_ = nullptr;
};

//=====================================================================================================================

class EncodingException : public eckit::Exception {
public:
    EncodingException(const std::string& reason, const eckit::CodeLocation& location = eckit::CodeLocation());
};

//=====================================================================================================================


}  // namespace action
}  // namespace multio
