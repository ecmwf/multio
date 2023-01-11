/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <iosfwd>

#include "multio/action/ChainedAction.h"
#include "multio/domain/Domain.h"
#include "multio/domain/Mask.h"

namespace multio {
namespace action {


class Mask : public ChainedAction {
public:
    explicit Mask(const ConfigurationContext& confCtx);

    void executeImpl(message::Message msg) const override;

private:
    template <typename T>
    message::Message createMasked(message::Message msg) const;

    template <typename T>
    void applyMask(message::Message msg) const;

    template <typename T>
    void applyOffset(message::Message msg) const;

    void print(std::ostream& os) const override;

    bool applyBitmap_;
    double missingValue_;
    std::set<std::string> offsetFields_;
    double offsetValue_;
};

}  // namespace action
}  // namespace multio
