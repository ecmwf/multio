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
#include <vector>
#include <set>
#include <iterator>

#include "eckit/utils/Optional.h"

#include "multio/action/ChainedAction.h"
#include "multio/message/MetadataSelector.h"

namespace multio {
namespace action {

//----------------------------------------------------------------------------------------------------------------------

class Select : public ChainedAction {
public:
    explicit Select(const ConfigurationContext& confCtx);

private: // methods

    void print(std::ostream &os) const override;

    bool matches(const message::Message& msg) const;

    void executeImpl(message::Message msg) const override;

    /// @note This describes an algebra, so the function here can be significantly extended to give helpful return
    void matchedFields(message::MetadataSelectors& selectors) const override;

private: // members

    message::MetadataSelectors selectors_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio
