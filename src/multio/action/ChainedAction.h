/*
 * (C) Copyright 2022- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Simon Smart
/// @date Nov 2022

#pragma once

#include "multio/action/Action.h"


namespace multio {
namespace action {

using util::ConfigurationContext;
using util::FailureAware;

//--------------------------------------------------------------------------------------------------

class ChainedAction : public Action {

    std::unique_ptr<Action> next_;

public:

    explicit ChainedAction(const ConfigurationContext& confCtx);
    ~ChainedAction() override = default;

protected:

    /// To be used from derived types
    void executeNext(message::Message msg) const;

    void activeFields(std::insert_iterator<std::set<std::string>>& ins) const;
    void activeCategories(std::insert_iterator<std::set<std::string>>& ins) const override;
};

//--------------------------------------------------------------------------------------------------


}  // namespace server
}  // namespace multio
