/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Action `mars2mars`: normalise the MARS keys of a field through metkit's mars2mars rules.
///
/// The action reads the MARS record from the message metadata, passes it through
/// `metkit::mars2mars::Mars2Mars::convert` and writes the changed keys back into the
/// metadata. The payload is never touched.
///
/// Configuration:
///
/// @code{.yaml}
/// - type: mars2mars
///   # Optional. Same syntax as the `select` action. When absent every field is converted.
///   selector:
///     match:
///       - param: [139, 170, 183, 236, 39, 40, 41, 42, 228246, 228247]
///         levtype: [sfc]
///   # Optional. Forwarded to the metkit API.
///   mars2mars-options:
///     printErrorStackToStdErr: false
/// @endcode
///
/// metkit applies *all* of its rules to a converted field. Some rules (local-to-WMO
/// parameter renames) also require the field values to be scaled. This action does
/// not transform values and fails loudly when such a rule fires; use the `selector`
/// to restrict the action to the fields that only need their keys renamed.

#pragma once

#include <memory>
#include <optional>

#include "metkit/mars2mars/api/Mars2Mars.h"

#include "multio/action/ChainedAction.h"
#include "multio/message/MetadataMatcher.h"

namespace multio::action::mars2mars {

//----------------------------------------------------------------------------------------------------------------------

class Mars2Mars : public ChainedAction {
public:
    explicit Mars2Mars(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;

    bool matches(const message::Message& msg) const;
    void applyInplace(message::Metadata& md);

    std::optional<message::match::MatchReduce> selector_;
    std::unique_ptr<metkit::mars2mars::Mars2Mars> converter_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action::mars2mars
